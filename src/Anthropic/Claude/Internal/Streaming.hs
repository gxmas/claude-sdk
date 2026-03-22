{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Anthropic.Claude.Internal.Streaming
Description : Internal streaming infrastructure
Copyright   : (c) 2026 Geoffrey Noël
License     : MIT
Stability   : internal

Connects SSE parsing with JSON decoding to produce a typed stream
of @Either APIError StreamEvent@ values.

Per ADR 0006, the stream return type @r@ carries the finalized
'MessageResponse' assembled from streaming events.
-}
module Anthropic.Claude.Internal.Streaming
  ( -- * Stream Construction
    createMessageStream

    -- * Stream Types
  , MessageStream

    -- * Internals (exported for testing)
  , decodeSSEEvent
  , updateMessageResponse
  , defaultMessageResponse
  ) where

import Anthropic.Claude.Internal.HTTP
import Anthropic.Claude.Internal.SSE
import Anthropic.Claude.Types.Client
import Anthropic.Claude.Types.Common
import Anthropic.Claude.Types.Core
import Anthropic.Claude.Types.Error
import Anthropic.Claude.Types.Logging (logHttpRequest, logHttpResponse)
import Anthropic.Claude.Types.Observability
import Anthropic.Claude.Types.Request
import Anthropic.Claude.Types.Response
import Anthropic.Claude.Types.Stream
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Network.HTTP.Client (RequestBody(..), BodyReader, brRead, brConsume)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.Status (statusCode)
import Streaming (Stream, Of)
import qualified Streaming.Prelude as S
import UnliftIO (MonadIO, liftIO)

-- | A stream of events that yields a finalized 'MessageResponse' at the end.
--
-- Per ADR 0006, the return type @r@ carries the assembled message.
type MessageStream m = Stream (Of (Either APIError StreamEvent)) m MessageResponse

-- | Create a streaming message request.
--
-- Sends a streaming request to the Claude API and returns a stream
-- of @Either APIError StreamEvent@ values. The stream's return value
-- is the assembled 'MessageResponse'.
--
-- The caller is responsible for consuming the stream within the
-- lifetime of the HTTP connection. Use @withMessageStream@ from
-- "Anthropic.Claude.Streaming" for automatic resource management.
createMessageStream
  :: MonadIO m
  => ClientEnv
  -> CreateMessageRequest
  -> IO (HTTP.Response BodyReader, MessageStream m)
createMessageStream env req = do
  let handler = clientEventHandler env
      logSettings = clientLogSettings env
      streamReq = req { requestStream = Just True }
      bodyBS = Aeson.encode streamReq
      path = "/v1/messages"
      method = "POST"
      pathTxt = T.pack path

  -- Emit request event + log
  startTime <- getCurrentTime
  emitEvent handler $ HttpRequest HttpRequestEvent
    { reqMethod = method, reqPath = pathTxt, reqTimestamp = startTime }
  logHttpRequest logSettings method pathTxt bodyBS

  httpReq <- buildRequest env methodPost path (RequestBodyLBS bodyBS)

  -- Open the response (keep connection open for streaming)
  httpResp <- HTTP.responseOpen httpReq (clientManager env)

  -- Emit response event + log (no body for streaming)
  endTime <- getCurrentTime
  let respHeaders = HTTP.responseHeaders httpResp
      reqId = lookup "request-id" respHeaders >>= \rid ->
        case TE.decodeUtf8' rid of
          Right txt -> Just (RequestId txt)
          Left _ -> Nothing
      rateInfo = extractRateLimitInfo respHeaders
      respStatus = HTTP.responseStatus httpResp
      status = statusCode respStatus
      duration = diffUTCTime endTime startTime
  emitEvent handler $ HttpResponse HttpResponseEvent
    { respStatusCode    = status
    , respDuration      = duration
    , respRequestId     = reqId
    , respRateLimitInfo = rateInfo
    }
  logHttpResponse logSettings method pathTxt status duration reqId rateInfo Nothing

  if status /= 200
    then do
      -- Non-200: read entire body for error
      chunks <- brConsume (HTTP.responseBody httpResp)
      let bodyLBS = LBS.fromChunks chunks
          errKind = errorKindFromStatus respStatus
          apiErr = case Aeson.eitherDecode bodyLBS of
            Left _  -> APIError errKind (ErrorDetails "unknown_error" "Failed to parse error response") status
            Right d -> APIError errKind d status
      let errStream = do
            S.yield (Left apiErr)
            pure defaultMessageResponse
      pure (httpResp, errStream)
    else do
      -- 200: read body and build stream
      let bodyReader = HTTP.responseBody httpResp
          stream = parseBodyReaderToStream bodyReader
      pure (httpResp, stream)
  where
    methodPost = "POST"

-- | Read SSE data from a 'BodyReader' and produce a typed stream.
--
-- Reads all data, parses SSE events, decodes JSON, yields each
-- event, and accumulates the final 'MessageResponse'.
parseBodyReaderToStream :: MonadIO m => BodyReader -> MessageStream m
parseBodyReaderToStream bodyReader = do
  -- Read all data from the body reader
  allBytes <- liftIO $ readAllChunks bodyReader
  let sseLines = BS8.lines allBytes
      sseEvents = buildSSEEvents sseLines
      decoded = concatMap maybeToList $ map decodeSSEEvent sseEvents

  -- Yield each event and accumulate the final message
  go defaultMessageResponse decoded
  where
    go acc [] = pure acc
    go acc (item : rest) = do
      S.yield item
      let acc' = case item of
            Right evt -> updateMessageResponse evt acc
            Left _    -> acc
      go acc' rest

    maybeToList Nothing  = []
    maybeToList (Just x) = [x]

-- | Read all bytes from a BodyReader.
readAllChunks :: BodyReader -> IO BS.ByteString
readAllChunks reader = BS.concat <$> go
  where
    go = do
      chunk <- brRead reader
      if BS.null chunk
        then pure []
        else (chunk :) <$> go

-- | Decode an SSE event into a StreamEvent or APIError.
decodeSSEEvent :: SSEEvent -> Maybe (Either APIError StreamEvent)
decodeSSEEvent (SSEEvent eventType eventData)
  | T.null eventData = Nothing
  | eventType == Just "ping" = Just (Right Ping)
  | otherwise = case Aeson.eitherDecode (LBS.fromStrict $ TE.encodeUtf8 eventData) of
      Left err -> Just $ Left $ APIError
        InvalidRequestError
        (ErrorDetails "parse_error" (T.pack err))
        200
      Right evt -> Just (Right evt)

-- | Update the accumulated MessageResponse with data from a StreamEvent.
updateMessageResponse :: StreamEvent -> MessageResponse -> MessageResponse
updateMessageResponse (MessageStart payload) _ = messageStartMessage payload
updateMessageResponse (ContentBlockStart payload) resp =
  let block = contentBlockStartBlock payload
  in resp { responseContent = responseContent resp ++ [block] }
updateMessageResponse (ContentBlockDelta payload) resp =
  case contentBlockDeltaDelta payload of
    TextDelta txt ->
      let idx = contentBlockDeltaIndex payload
          content' = updateContentAt idx (appendText txt) (responseContent resp)
      in resp { responseContent = content' }
    InputJsonDelta _ -> resp
updateMessageResponse (MessageDelta payload) resp =
  let delta = messageDeltaDelta payload
      usage = messageDeltaUsage payload
  in resp
    { responseStopReason   = deltaStopReason delta
    , responseStopSequence = deltaStopSequence delta
    , responseUsage        = usage
    }
updateMessageResponse _ resp = resp

-- | Append text to a TextBlock.
appendText :: T.Text -> ContentBlock -> ContentBlock
appendText txt (TextBlock existing) = TextBlock (existing <> txt)
appendText _ block = block

-- | Update the content block at a specific index.
updateContentAt :: Int -> (ContentBlock -> ContentBlock) -> [ContentBlock] -> [ContentBlock]
updateContentAt _ _ [] = []
updateContentAt 0 f (x:xs) = f x : xs
updateContentAt n f (x:xs) = x : updateContentAt (n-1) f xs

-- | Default empty message response used as accumulator starting point.
defaultMessageResponse :: MessageResponse
defaultMessageResponse = MessageResponse
  { responseId           = MessageId ""
  , responseType         = "message"
  , responseRole         = Assistant
  , responseContent      = []
  , responseModel        = ModelId ""
  , responseStopReason   = Nothing
  , responseStopSequence = Nothing
  , responseUsage        = Usage 0 0
  }
