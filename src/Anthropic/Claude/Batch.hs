{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Anthropic.Claude.Batch
Description : Batch API operations
Copyright   : (c) 2026 Geoffrey Noël
License     : MIT
Maintainer  : noel.geoff@gmail.com

Operations for the Claude Message Batches API. Batches allow
processing multiple message requests asynchronously, with lower
costs compared to synchronous processing.

= Usage

1. Create a batch with 'createBatch'
2. Poll status with 'retrieveBatch' or use 'pollBatchUntilDone'
3. Download results from the URL when status is 'Ended'
4. Optionally cancel with 'cancelBatch' if needed

Batches process requests asynchronously and provide results via
a downloadable JSONL file when complete.
-}
module Anthropic.Claude.Batch
  ( -- * Batch Operations
    createBatch
  , retrieveBatch
  , listBatches
  , cancelBatch

    -- * Batch Results
  , pollBatchUntilDone
  ) where

import Anthropic.Claude.Internal.HTTP
import Anthropic.Claude.Internal.Retry
import Anthropic.Claude.Types.Batch
import Anthropic.Claude.Types.Client
import Anthropic.Claude.Types.Core
import Anthropic.Claude.Types.Error
import Anthropic.Claude.Types.Logging (logHttpRequest, logHttpResponse)
import Anthropic.Claude.Types.Observability
import Anthropic.Claude.Types.Response (APIResponse(..))
import Control.Concurrent (threadDelay)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (NominalDiffTime, getCurrentTime, diffUTCTime)
import Network.HTTP.Client (RequestBody(..))
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Status (statusCode)
import UnliftIO (MonadUnliftIO, liftIO)

-- | Create a new message batch.
createBatch
  :: MonadUnliftIO m
  => ClientEnv
  -> CreateBatchRequest
  -> m (Either APIError (APIResponse BatchResponse))
createBatch env req = withRetry env $ liftIO $ do
  let handler = clientEventHandler env
      logSettings = clientLogSettings env
      path = "/v1/messages/batches"
      method = "POST"
      pathTxt = T.pack path
  let bodyBS = Aeson.encode req

  startTime <- getCurrentTime
  emitEvent handler $ HttpRequest HttpRequestEvent
    { reqMethod = method, reqPath = pathTxt, reqTimestamp = startTime }
  logHttpRequest logSettings method pathTxt bodyBS

  httpReq <- buildRequest env "POST" path (RequestBodyLBS bodyBS)
  httpResp <- executeRequest (clientManager env) httpReq
  let headers = HTTP.responseHeaders httpResp
      reqId = extractRequestId headers
      rateInfo = extractRateLimitInfo headers
      status = statusCode (HTTP.responseStatus httpResp)
      respBody = HTTP.responseBody httpResp

  endTime <- getCurrentTime
  let duration = diffUTCTime endTime startTime
  emitEvent handler $ HttpResponse HttpResponseEvent
    { respStatusCode = status, respDuration = duration
    , respRequestId = reqId, respRateLimitInfo = rateInfo }
  logHttpResponse logSettings method pathTxt status duration reqId rateInfo (Just respBody)

  case parseResponse httpResp reqId of
    Left err -> pure $ Left err
    Right body -> pure $ Right $ APIResponse body rateInfo reqId

-- | Retrieve a batch by ID.
retrieveBatch
  :: MonadUnliftIO m
  => ClientEnv
  -> BatchId
  -> m (Either APIError (APIResponse BatchResponse))
retrieveBatch env (BatchId bid) = withRetry env $ liftIO $ do
  let handler = clientEventHandler env
      logSettings = clientLogSettings env
      path = "/v1/messages/batches/" <> T.unpack bid
      method = "GET"
      pathTxt = T.pack path

  startTime <- getCurrentTime
  emitEvent handler $ HttpRequest HttpRequestEvent
    { reqMethod = method, reqPath = pathTxt, reqTimestamp = startTime }
  logHttpRequest logSettings method pathTxt ""

  httpReq <- buildRequest env "GET" path (RequestBodyBS "")
  httpResp <- executeRequest (clientManager env) httpReq
  let headers = HTTP.responseHeaders httpResp
      reqId = extractRequestId headers
      rateInfo = extractRateLimitInfo headers
      status = statusCode (HTTP.responseStatus httpResp)
      respBody = HTTP.responseBody httpResp

  endTime <- getCurrentTime
  let duration = diffUTCTime endTime startTime
  emitEvent handler $ HttpResponse HttpResponseEvent
    { respStatusCode = status, respDuration = duration
    , respRequestId = reqId, respRateLimitInfo = rateInfo }
  logHttpResponse logSettings method pathTxt status duration reqId rateInfo (Just respBody)

  case parseResponse httpResp reqId of
    Left err -> pure $ Left err
    Right body -> pure $ Right $ APIResponse body rateInfo reqId

-- | List all batches.
listBatches
  :: MonadUnliftIO m
  => ClientEnv
  -> m (Either APIError (APIResponse [BatchResponse]))
listBatches env = withRetry env $ liftIO $ do
  let handler = clientEventHandler env
      logSettings = clientLogSettings env
      path = "/v1/messages/batches"
      method = "GET"
      pathTxt = T.pack path

  startTime <- getCurrentTime
  emitEvent handler $ HttpRequest HttpRequestEvent
    { reqMethod = method, reqPath = pathTxt, reqTimestamp = startTime }
  logHttpRequest logSettings method pathTxt ""

  httpReq <- buildRequest env "GET" path (RequestBodyBS "")
  httpResp <- executeRequest (clientManager env) httpReq
  let headers = HTTP.responseHeaders httpResp
      reqId = extractRequestId headers
      rateInfo = extractRateLimitInfo headers
      status = statusCode (HTTP.responseStatus httpResp)
      respBody = HTTP.responseBody httpResp

  endTime <- getCurrentTime
  let duration = diffUTCTime endTime startTime
  emitEvent handler $ HttpResponse HttpResponseEvent
    { respStatusCode = status, respDuration = duration
    , respRequestId = reqId, respRateLimitInfo = rateInfo }
  logHttpResponse logSettings method pathTxt status duration reqId rateInfo (Just respBody)

  case parseResponse httpResp reqId of
    Left err -> pure $ Left err
    Right body -> pure $ Right $ APIResponse body rateInfo reqId

-- | Cancel a batch.
cancelBatch
  :: MonadUnliftIO m
  => ClientEnv
  -> BatchId
  -> m (Either APIError (APIResponse BatchResponse))
cancelBatch env (BatchId bid) = withRetry env $ liftIO $ do
  let handler = clientEventHandler env
      logSettings = clientLogSettings env
      path = "/v1/messages/batches/" <> T.unpack bid <> "/cancel"
      method = "POST"
      pathTxt = T.pack path

  startTime <- getCurrentTime
  emitEvent handler $ HttpRequest HttpRequestEvent
    { reqMethod = method, reqPath = pathTxt, reqTimestamp = startTime }
  logHttpRequest logSettings method pathTxt ""

  httpReq <- buildRequest env "POST" path (RequestBodyBS "")
  httpResp <- executeRequest (clientManager env) httpReq
  let headers = HTTP.responseHeaders httpResp
      reqId = extractRequestId headers
      rateInfo = extractRateLimitInfo headers
      status = statusCode (HTTP.responseStatus httpResp)
      respBody = HTTP.responseBody httpResp

  endTime <- getCurrentTime
  let duration = diffUTCTime endTime startTime
  emitEvent handler $ HttpResponse HttpResponseEvent
    { respStatusCode = status, respDuration = duration
    , respRequestId = reqId, respRateLimitInfo = rateInfo }
  logHttpResponse logSettings method pathTxt status duration reqId rateInfo (Just respBody)

  case parseResponse httpResp reqId of
    Left err -> pure $ Left err
    Right body -> pure $ Right $ APIResponse body rateInfo reqId

-- | Poll a batch until it reaches 'Ended' status.
--
-- Continuously polls 'retrieveBatch' at the specified interval until
-- the batch processing completes. Returns the final batch response
-- with @resultsUrl@ populated, or the first error encountered.
--
-- Note: This function blocks until the batch completes. For
-- long-running batches, consider using a background thread.
--
-- Example:
-- @
-- result <- pollBatchUntilDone env batchId 5.0  -- Poll every 5 seconds
-- case result of
--   Left err -> handleError err
--   Right batch -> downloadResults (batchResponseResultsUrl batch)
-- @
pollBatchUntilDone
  :: MonadUnliftIO m
  => ClientEnv
  -> BatchId
  -> NominalDiffTime    -- ^ Poll interval in seconds
  -> m (Either APIError BatchResponse)
pollBatchUntilDone env bid interval = go
  where
    go = do
      result <- retrieveBatch env bid
      case result of
        Left err -> pure $ Left err
        Right resp ->
          let batch = apiResponseBody resp
          in case batchResponseProcessingStatus batch of
            Ended -> pure $ Right batch
            _ -> do
              liftIO $ threadDelay (round (interval * 1000000))
              go

-- | Extract request ID from response headers.
extractRequestId :: [Header] -> Maybe RequestId
extractRequestId headers = do
  value <- lookup "request-id" headers
  case TE.decodeUtf8' value of
    Right txt -> Just (RequestId txt)
    Left _ -> Nothing
