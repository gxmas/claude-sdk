{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Anthropic.Claude.StreamingSpec (spec) where

import Anthropic.Claude.Internal.SSE (buildSSEEvents)
import Anthropic.Claude.Internal.Streaming (decodeSSEEvent, defaultMessageResponse, updateMessageResponse)
import Anthropic.Claude.Streaming
import Anthropic.Claude.TestHelpers
import Anthropic.Claude.Types.ContentBlock
import Anthropic.Claude.Types.Core
import Anthropic.Claude.Types.Error
import Anthropic.Claude.Types.Response
import Anthropic.Claude.Types.Stream
import Control.Exception (SomeException, try)
import Data.Maybe (mapMaybe)
import qualified Streaming.Prelude as S
import Test.Hspec

spec :: Spec
spec = describe "Streaming" $ do
  describe "throwOnError" $ do
    it "passes Right values through" $ do
      let stream = do
            S.yield (Right Ping)
            S.yield (Right MessageStop)
            pure defaultMessageResponse
          cleanStream = throwOnError stream
      events <- S.toList_ cleanStream
      events `shouldBe` [Ping, MessageStop]

    it "throws on Left value" $ do
      let err = APIError ServerError (ErrorDetails "api_error" "fail") 500
          stream = do
            S.yield (Left err)
            pure defaultMessageResponse
          cleanStream = throwOnError stream
      result <- try (S.toList_ cleanStream)
      case result of
        Left (_ :: SomeException) -> pure ()
        Right _ -> expectationFailure "Expected exception"

  describe "forEachEvent (mock SSE pipeline)" $ do
    it "processes a complete SSE stream into events" $ do
      let sseEvents = buildSSEEvents mockSSELines
          decoded = mapMaybe decodeSSEEvent sseEvents
          rightEvents = [evt | Right evt <- decoded]

      -- Should have 6 events: message_start, content_block_start, content_block_delta, content_block_stop, message_delta, message_stop
      length rightEvents `shouldBe` 6

      -- First event is MessageStart
      case head rightEvents of
        MessageStart payload -> responseId (messageStartMessage payload) `shouldBe` MessageId "msg_s1"
        other -> expectationFailure $ "Expected MessageStart, got: " ++ show other

      -- Third event is ContentBlockDelta with "Hello!"
      case rightEvents !! 2 of
        ContentBlockDelta payload -> contentBlockDeltaDelta payload `shouldBe` TextDelta "Hello!"
        other -> expectationFailure $ "Expected ContentBlockDelta, got: " ++ show other

      -- Last event is MessageStop
      last rightEvents `shouldBe` MessageStop

    it "accumulates events into final MessageResponse" $ do
      let sseEvents = buildSSEEvents mockSSELines
          decoded = mapMaybe decodeSSEEvent sseEvents
          rightEvents = [evt | Right evt <- decoded]
          finalMsg = foldl (flip updateMessageResponse) defaultMessageResponse rightEvents

      responseId finalMsg `shouldBe` MessageId "msg_s1"
      responseContent finalMsg `shouldBe` [TextBlock "Hello!" Nothing Nothing]
      responseStopReason finalMsg `shouldBe` Just EndTurn
      usageOutputTokens (responseUsage finalMsg) `shouldBe` 5

  describe "withMessageStream (mock SSE pipeline)" $ do
    it "builds a stream that yields events and returns final message" $ do
      let sseEvents = buildSSEEvents mockSSELines
          decoded = mapMaybe decodeSSEEvent sseEvents

          -- Simulate the stream that createMessageStream would produce
          mockStream = go defaultMessageResponse decoded
          go acc [] = pure acc
          go acc (item : rest) = do
            S.yield item
            let acc' = case item of
                  Right evt -> updateMessageResponse evt acc
                  Left _ -> acc
            go acc' rest

      -- Consume the stream
      (events, finalMsg) <- do
        evts <- S.toList_ mockStream
        -- Rebuild final message manually
        let rightEvts = [evt | Right evt <- evts]
            final = foldl (flip updateMessageResponse) defaultMessageResponse rightEvts
        pure (evts, final)

      -- Verify events
      length events `shouldBe` 6
      -- Verify final message
      extractText finalMsg `shouldBe` "Hello!"
      responseStopReason finalMsg `shouldBe` Just EndTurn
