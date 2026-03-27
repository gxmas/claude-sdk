{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Anthropic.Claude.Messages
Description : Messages API operations
Copyright   : (c) 2026 Geoffrey Noël
License     : MIT
Maintainer  : noel.geoff@gmail.com

Public API for creating messages with Claude.
Handles request construction, HTTP execution, and response parsing.

This module provides the core 'createMessage' operation for synchronous
(non-streaming) message creation. For streaming responses, use
"Anthropic.Claude.Streaming" instead.

All requests are automatically retried on transient errors (429, 500, 529)
according to the retry policy configured in 'ClientEnv'.
-}
module Anthropic.Claude.Messages
  ( createMessage
  ) where

import Anthropic.Claude.Internal.HTTP
import Anthropic.Claude.Internal.Retry
import Anthropic.Claude.Types.Client
import Anthropic.Claude.Types.Core
import Anthropic.Claude.Types.Error
import Anthropic.Claude.Types.Logging (logHttpRequest, logHttpResponse)
import Anthropic.Claude.Types.Observability
import Anthropic.Claude.Types.Request
import Anthropic.Claude.Types.Response
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Network.HTTP.Client (RequestBody (..))
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.Method (methodPost)
import Network.HTTP.Types.Status (statusCode)
import UnliftIO (MonadUnliftIO, liftIO)

-- | Create a message
--
-- Sends a request to the Messages API and returns either an APIError
-- or the response wrapped in APIResponse with rate limit metadata.
--
-- Per ADR 0002: API errors (4xx, 5xx) are returned in Either,
-- network errors are thrown as exceptions.
--
-- Per ADR 0003: Automatically retries transient errors (429, 500, 529)
-- using the retry policy from ClientEnv.
--
-- Example:
-- @
-- let req = mkRequest claude4Sonnet [userMsg "Hello!"] 1024
-- result <- createMessage env req
-- case result of
--   Left err -> print err
--   Right response -> do
--     let msg = apiResponseBody response
--     print (extractText msg)
-- @
createMessage
  :: MonadUnliftIO m
  => ClientEnv
  -> CreateMessageRequest
  -> m (Either APIError (APIResponse MessageResponse))
createMessage env req = withRetry env $ liftIO $ do
  let handler = clientEventHandler env
      logSettings = clientLogSettings env
      path = "/v1/messages"
      methodTxt = "POST"
      pathTxt = T.pack path

  -- Build request body
  let bodyBS = Aeson.encode req

  -- Emit request event + log
  startTime <- getCurrentTime
  emitEvent handler
    $ HttpRequest
      HttpRequestEvent
        { reqMethod = methodTxt
        , reqPath = pathTxt
        , reqTimestamp = startTime
        }
  logHttpRequest logSettings methodTxt pathTxt bodyBS

  -- Execute request
  httpReq <- buildRequest env methodPost path (RequestBodyLBS bodyBS)
  httpResp <- executeRequest (clientManager env) httpReq

  -- Extract request ID from headers
  let headers = HTTP.responseHeaders httpResp
      requestId =
        lookup "request-id" headers >>= \rid ->
          case TE.decodeUtf8' rid of
            Right txt -> Just (RequestId txt)
            Left _ -> Nothing
      rateLimitInfo = extractRateLimitInfo headers
      respStatus = statusCode (HTTP.responseStatus httpResp)
      respBody = HTTP.responseBody httpResp

  -- Emit response event + log
  endTime <- getCurrentTime
  let duration = diffUTCTime endTime startTime
  emitEvent handler
    $ HttpResponse
      HttpResponseEvent
        { respStatusCode = respStatus
        , respDuration = duration
        , respRequestId = requestId
        , respRateLimitInfo = rateLimitInfo
        }
  logHttpResponse
    logSettings
    methodTxt
    pathTxt
    respStatus
    duration
    requestId
    rateLimitInfo
    (Just respBody)

  -- Parse response
  case parseResponse httpResp requestId of
    Left apiError -> pure $ Left apiError
    Right msgResponse ->
      pure
        $ Right
        $ APIResponse
          { apiResponseBody = msgResponse
          , apiResponseRateLimitInfo = rateLimitInfo
          , apiResponseRequestId = requestId
          }
