{-# LANGUAGE OverloadedStrings #-}

module Anthropic.Claude.Internal.StreamingSpec (spec) where

import Anthropic.Claude.Internal.SSE
import Anthropic.Claude.Internal.Streaming
import Anthropic.Claude.Types.ContentBlock
import Anthropic.Claude.Types.Core
import Anthropic.Claude.Types.Error
import Anthropic.Claude.Types.Response
import Anthropic.Claude.Types.Stream
import Test.Hspec

spec :: Spec
spec = describe "Internal.Streaming" $ do
  describe "decodeSSEEvent" $ do
    it "decodes valid JSON into StreamEvent" $ do
      let sse = SSEEvent (Just "message_stop") "{\"type\":\"message_stop\"}"
      decodeSSEEvent sse `shouldBe` Just (Right MessageStop)

    it "decodes ping events" $ do
      let sse = SSEEvent (Just "ping") "{}"
      decodeSSEEvent sse `shouldBe` Just (Right Ping)

    it "returns Left for invalid JSON" $ do
      let sse = SSEEvent (Just "message_start") "not json"
      case decodeSSEEvent sse of
        Just (Left apiErr) -> errorKind apiErr `shouldBe` InvalidRequestError
        _ -> expectationFailure "Expected Left APIError"

    it "returns Nothing for empty data" $ do
      let sse = SSEEvent Nothing ""
      decodeSSEEvent sse `shouldBe` Nothing

  describe "updateMessageResponse" $ do
    it "MessageStart replaces the response" $ do
      let msg =
            MessageResponse
              (MessageId "msg_123")
              "message"
              Assistant
              [TextBlock "hello" Nothing]
              (ModelId "claude")
              (Just EndTurn)
              Nothing
              (Usage 10 20 Nothing Nothing)
          evt = MessageStart (MessageStartPayload msg)
          result = updateMessageResponse evt defaultMessageResponse
      responseId result `shouldBe` MessageId "msg_123"
      responseContent result `shouldBe` [TextBlock "hello" Nothing]

    it "ContentBlockStart appends a block" $ do
      let evt = ContentBlockStart (ContentBlockStartPayload 0 (TextBlock "" Nothing))
          result = updateMessageResponse evt defaultMessageResponse
      length (responseContent result) `shouldBe` 1

    it "ContentBlockDelta appends text" $ do
      let base = defaultMessageResponse {responseContent = [TextBlock "He" Nothing]}
          evt = ContentBlockDelta (ContentBlockDeltaPayload 0 (TextDelta "llo"))
          result = updateMessageResponse evt base
      responseContent result `shouldBe` [TextBlock "Hello" Nothing]

    it "MessageDelta sets stop reason and usage" $ do
      let delta = DeltaBody (Just EndTurn) Nothing
          usage = Usage 100 200 Nothing Nothing
          evt = MessageDelta (MessageDeltaPayload delta usage)
          result = updateMessageResponse evt defaultMessageResponse
      responseStopReason result `shouldBe` Just EndTurn
      responseUsage result `shouldBe` usage

    it "other events leave response unchanged" $ do
      let result = updateMessageResponse Ping defaultMessageResponse
      result `shouldBe` defaultMessageResponse
