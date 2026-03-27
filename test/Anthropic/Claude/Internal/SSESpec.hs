{-# LANGUAGE OverloadedStrings #-}

module Anthropic.Claude.Internal.SSESpec (spec) where

import Anthropic.Claude.Internal.SSE
import Test.Hspec

spec :: Spec
spec = describe "Internal.SSE" $ do
  describe "parseSSELine" $ do
    it "returns Nothing for empty line"
      $ parseSSELine "" `shouldBe` Nothing

    it "parses data field"
      $ parseSSELine "data: hello world" `shouldBe` Just (DataField "hello world")

    it "parses data field without space after colon"
      $ parseSSELine "data:hello" `shouldBe` Just (DataField "hello")

    it "parses event field"
      $ parseSSELine "event: message_start" `shouldBe` Just (EventField "message_start")

    it "parses comment"
      $ parseSSELine ": this is a comment" `shouldBe` Just (CommentField " this is a comment")

    it "parses empty comment (keep-alive)"
      $ parseSSELine ":" `shouldBe` Just (CommentField "")

    it "parses retry field"
      $ parseSSELine "retry: 3000" `shouldBe` Just (RetryField "3000")

    it "parses id field"
      $ parseSSELine "id: 123" `shouldBe` Just (IdField "123")

    it "parses data with JSON content" $ do
      let line = "data: {\"type\":\"message_start\",\"message\":{\"id\":\"msg_123\"}}"
      case parseSSELine line of
        Just (DataField d) -> d `shouldBe` "{\"type\":\"message_start\",\"message\":{\"id\":\"msg_123\"}}"
        _ -> expectationFailure "Expected DataField"

    it "handles data with colons in value" $ do
      parseSSELine "data: key:value:more" `shouldBe` Just (DataField "key:value:more")

  describe "buildSSEEvents" $ do
    it "builds single event from data + empty line" $ do
      let input = ["data: hello", ""]
      buildSSEEvents input `shouldBe` [SSEEvent Nothing "hello"]

    it "builds event with event type" $ do
      let input = ["event: message_start", "data: {}", ""]
      buildSSEEvents input `shouldBe` [SSEEvent (Just "message_start") "{}"]

    it "concatenates multi-line data with newlines" $ do
      let input = ["data: line1", "data: line2", "data: line3", ""]
      buildSSEEvents input `shouldBe` [SSEEvent Nothing "line1\nline2\nline3"]

    it "builds multiple events separated by empty lines" $ do
      let input = ["data: first", "", "data: second", ""]
      buildSSEEvents input
        `shouldBe` [ SSEEvent Nothing "first"
                   , SSEEvent Nothing "second"
                   ]

    it "skips comments" $ do
      let input = [": comment", "data: hello", ""]
      buildSSEEvents input `shouldBe` [SSEEvent Nothing "hello"]

    it "handles ping events" $ do
      let input = ["event: ping", "data: {}", ""]
      buildSSEEvents input `shouldBe` [SSEEvent (Just "ping") "{}"]

    it "handles empty input"
      $ buildSSEEvents [] `shouldBe` []

    it "flushes remaining data at end of input" $ do
      let input = ["data: no trailing newline"]
      buildSSEEvents input `shouldBe` [SSEEvent Nothing "no trailing newline"]

    it "skips empty data groups" $ do
      let input = ["", "", "data: hello", ""]
      buildSSEEvents input `shouldBe` [SSEEvent Nothing "hello"]

    it "handles realistic Claude API stream" $ do
      let input =
            [ "event: message_start"
            , "data: {\"type\":\"message_start\",\"message\":{\"id\":\"msg_1\"}}"
            , ""
            , "event: content_block_start"
            , "data: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}"
            , ""
            , "event: content_block_delta"
            , "data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"Hi\"}}"
            , ""
            , "event: message_stop"
            , "data: {\"type\":\"message_stop\"}"
            , ""
            ]
      let events = buildSSEEvents input
      length events `shouldBe` 4
      sseEventType (head events) `shouldBe` Just "message_start"
      sseEventType (events !! 2) `shouldBe` Just "content_block_delta"
