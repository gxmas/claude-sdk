{-# LANGUAGE OverloadedStrings #-}

{- |
Shared test helpers for mock HTTP responses.
-}
module Anthropic.Claude.TestHelpers
  ( -- * Mock HTTP Responses
    mkMockResponse
  , mockSuccess
  , mockError

    -- * Sample JSON Bodies
  , sampleMessageResponseJson
  , sampleBatchResponseJson
  , sampleBatchEndedJson

    -- * Mock SSE Data
  , mockSSELines
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Client.Internal (CookieJar (..), Response (..), ResponseClose (..))
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Types.Status (mkStatus)
import Network.HTTP.Types.Version (http11)

-- | Construct a mock HTTP Response with the given status, headers, and body.
mkMockResponse :: Int -> ResponseHeaders -> LBS.ByteString -> Response LBS.ByteString
mkMockResponse code headers body =
  Response
    { responseStatus = mkStatus code ""
    , responseVersion = http11
    , responseHeaders = headers
    , responseBody = body
    , responseCookieJar = CJ []
    , responseClose' = ResponseClose (pure ())
    , responseOriginalRequest = error "mock: responseOriginalRequest not available"
    , responseEarlyHints = []
    }

-- | Construct a mock 200 response with optional rate limit headers.
mockSuccess :: LBS.ByteString -> Response LBS.ByteString
mockSuccess body =
  mkMockResponse
    200
    [ ("request-id", "req_mock_123")
    , ("anthropic-ratelimit-requests-limit", "50")
    , ("anthropic-ratelimit-requests-remaining", "49")
    ]
    body

-- | Construct a mock error response.
mockError :: Int -> String -> String -> Response LBS.ByteString
mockError code errType errMsg =
  mkMockResponse
    code
    []
    ( Aeson.encode
        $ Aeson.object
          [ "error"
              Aeson..= Aeson.object
                [ "type" Aeson..= errType
                , "message" Aeson..= errMsg
                ]
          ]
    )

-- | Sample successful MessageResponse JSON.
sampleMessageResponseJson :: LBS.ByteString
sampleMessageResponseJson =
  Aeson.encode
    $ Aeson.object
      [ "id" Aeson..= ("msg_test_123" :: String)
      , "type" Aeson..= ("message" :: String)
      , "role" Aeson..= ("assistant" :: String)
      , "content"
          Aeson..= [ Aeson.object
                       [ "type" Aeson..= ("text" :: String)
                       , "text" Aeson..= ("Hello! How can I help you?" :: String)
                       ]
                   ]
      , "model" Aeson..= ("claude-sonnet-4-6" :: String)
      , "stop_reason" Aeson..= ("end_turn" :: String)
      , "usage"
          Aeson..= Aeson.object
            [ "input_tokens" Aeson..= (15 :: Int)
            , "output_tokens" Aeson..= (8 :: Int)
            ]
      ]

-- | Sample BatchResponse JSON (in_progress).
sampleBatchResponseJson :: LBS.ByteString
sampleBatchResponseJson =
  Aeson.encode
    $ Aeson.object
      [ "id" Aeson..= ("batch_test_456" :: String)
      , "type" Aeson..= ("message_batch" :: String)
      , "processing_status" Aeson..= ("in_progress" :: String)
      , "request_counts"
          Aeson..= Aeson.object
            [ "processing" Aeson..= (5 :: Int)
            , "succeeded" Aeson..= (0 :: Int)
            , "errored" Aeson..= (0 :: Int)
            , "canceled" Aeson..= (0 :: Int)
            , "expired" Aeson..= (0 :: Int)
            ]
      , "created_at" Aeson..= ("2026-03-21T00:00:00Z" :: String)
      ]

-- | Sample BatchResponse JSON (ended).
sampleBatchEndedJson :: LBS.ByteString
sampleBatchEndedJson =
  Aeson.encode
    $ Aeson.object
      [ "id" Aeson..= ("batch_test_456" :: String)
      , "type" Aeson..= ("message_batch" :: String)
      , "processing_status" Aeson..= ("ended" :: String)
      , "request_counts"
          Aeson..= Aeson.object
            [ "processing" Aeson..= (0 :: Int)
            , "succeeded" Aeson..= (5 :: Int)
            , "errored" Aeson..= (0 :: Int)
            , "canceled" Aeson..= (0 :: Int)
            , "expired" Aeson..= (0 :: Int)
            ]
      , "created_at" Aeson..= ("2026-03-21T00:00:00Z" :: String)
      , "ended_at" Aeson..= ("2026-03-21T00:01:00Z" :: String)
      ]

-- | Mock SSE lines for a complete streaming response.
mockSSELines :: [BS.ByteString]
mockSSELines =
  [ "event: message_start"
  , "data: {\"type\":\"message_start\",\"message\":{\"id\":\"msg_s1\",\"type\":\"message\",\"role\":\"assistant\",\"content\":[],\"model\":\"claude-sonnet-4-6\",\"usage\":{\"input_tokens\":10,\"output_tokens\":0}}}"
  , ""
  , "event: content_block_start"
  , "data: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}"
  , ""
  , "event: content_block_delta"
  , "data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"Hello!\"}}"
  , ""
  , "event: content_block_stop"
  , "data: {\"type\":\"content_block_stop\",\"index\":0}"
  , ""
  , "event: message_delta"
  , "data: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"input_tokens\":10,\"output_tokens\":5}}"
  , ""
  , "event: message_stop"
  , "data: {\"type\":\"message_stop\"}"
  , ""
  ]
