{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Anthropic.Claude.Messages
Description : Messages API operations
Copyright   : (c) 2026 Anthropic
License     : MIT
Maintainer  : gnoel@anthropic.com

Public API for creating messages with Claude.
Handles request construction, HTTP execution, and response parsing.
-}
module Anthropic.Claude.Messages
  ( createMessage
  ) where

import Anthropic.Claude.Internal.HTTP
import Anthropic.Claude.Types.Client
import Anthropic.Claude.Types.Core
import Anthropic.Claude.Types.Error
import Anthropic.Claude.Types.Request
import Anthropic.Claude.Types.Response
import qualified Data.Aeson as Aeson
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client (RequestBody(..))
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.Method (methodPost)
import UnliftIO (MonadUnliftIO, liftIO)

-- | Create a message
--
-- Sends a request to the Messages API and returns either an APIError
-- or the response wrapped in APIResponse with rate limit metadata.
--
-- Per ADR 0002: API errors (4xx, 5xx) are returned in Either,
-- network errors are thrown as exceptions.
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
createMessage env req = liftIO $ do
  -- Build request
  let bodyBS = Aeson.encode req
  httpReq <- buildRequest env methodPost "/v1/messages" (RequestBodyLBS bodyBS)

  -- Execute request
  httpResp <- executeRequest (clientManager env) httpReq

  -- Extract request ID from headers
  let headers = HTTP.responseHeaders httpResp
      requestId = lookup "request-id" headers >>= \rid ->
        case TE.decodeUtf8' rid of
          Right txt -> Just (RequestId txt)
          Left _ -> Nothing

  -- Parse response
  case parseResponse httpResp requestId of
    Left apiError -> pure $ Left apiError
    Right msgResponse -> do
      let rateLimitInfo = extractRateLimitInfo headers
      pure $ Right $ APIResponse
        { apiResponseBody = msgResponse
        , apiResponseRateLimitInfo = rateLimitInfo
        , apiResponseRequestId = requestId
        }
