{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Anthropic.Claude.Types.StreamSpec (spec) where

import Anthropic.Claude.Types.ContentBlock (Citation (..))
import Anthropic.Claude.Types.Core
import Anthropic.Claude.Types.Response
import Anthropic.Claude.Types.Stream
import Data.Aeson (decode, encode)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

-- Arbitrary instances

genText :: Gen T.Text
genText = T.pack <$> listOf1 (elements ['a' .. 'z'])

instance Arbitrary Citation where
  arbitrary =
    oneof
      [ CharLocation <$> genText <*> choose (0, 10) <*> pure Nothing <*> choose (0, 100) <*> choose (0, 100)
      , PageLocation <$> genText <*> choose (0, 10) <*> pure Nothing <*> choose (1, 50) <*> choose (1, 50)
      , ContentBlockLocation <$> genText <*> choose (0, 10) <*> pure Nothing <*> choose (0, 20) <*> choose (0, 20)
      ]

instance Arbitrary ContentDelta where
  arbitrary =
    oneof
      [ TextDelta <$> genText
      , InputJsonDelta <$> genText
      , ThinkingDelta <$> genText
      , SignatureDelta <$> genText
      , CitationsDelta <$> arbitrary
      ]

instance Arbitrary Usage where
  arbitrary = Usage <$> choose (0, 10000) <*> choose (0, 10000) <*> pure Nothing <*> pure Nothing

instance Arbitrary DeltaBody where
  arbitrary =
    DeltaBody
      <$> (Just <$> elements [EndTurn, MaxTokens, StopSequence, ToolUse])
      <*> (Just <$> genText)

instance Arbitrary MessageDeltaPayload where
  arbitrary = MessageDeltaPayload <$> arbitrary <*> arbitrary

instance Arbitrary ContentBlockDeltaPayload where
  arbitrary = ContentBlockDeltaPayload <$> choose (0, 10) <*> arbitrary

-- Tests

spec :: Spec
spec = describe "Types.Stream" $ do
  describe "ContentDelta" $ do
    it "parses TextDelta from JSON" $ do
      let json = "{\"type\":\"text_delta\",\"text\":\"Hello\"}"
      decode json `shouldBe` Just (TextDelta "Hello")

    it "parses InputJsonDelta from JSON" $ do
      let json = "{\"type\":\"input_json_delta\",\"partial_json\":\"{\\\"key\\\"\"}"
      decode json `shouldBe` Just (InputJsonDelta "{\"key\"")

    it "parses ThinkingDelta from JSON" $ do
      let json = "{\"type\":\"thinking_delta\",\"thinking\":\"Let me think...\"}"
      decode json `shouldBe` Just (ThinkingDelta "Let me think...")

    it "parses SignatureDelta from JSON" $ do
      let json = "{\"type\":\"signature_delta\",\"signature\":\"sig_abc123\"}"
      decode json `shouldBe` Just (SignatureDelta "sig_abc123")

    it "parses CitationsDelta from JSON" $ do
      let json = "{\"type\":\"citations_delta\",\"citation\":{\"type\":\"char_location\",\"cited_text\":\"hello\",\"document_index\":0,\"start_char_index\":0,\"end_char_index\":5}}"
      decode json `shouldBe` Just (CitationsDelta (CharLocation "hello" 0 Nothing 0 5))

    it "round-trips through JSON"
      $ property
      $ \(d :: ContentDelta) -> decode (encode d) === Just d

  describe "StreamEvent" $ do
    it "parses message_stop" $ do
      let json = "{\"type\":\"message_stop\"}"
      decode json `shouldBe` Just MessageStop

    it "parses ping" $ do
      let json = "{\"type\":\"ping\"}"
      decode json `shouldBe` Just Ping

    it "parses content_block_stop" $ do
      let json = "{\"type\":\"content_block_stop\",\"index\":0}"
      decode json `shouldBe` Just (ContentBlockStop 0)

    it "parses content_block_delta with text" $ do
      let json = "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"Hi\"}}"
      case decode json of
        Just (ContentBlockDelta p) -> do
          contentBlockDeltaIndex p `shouldBe` 0
          contentBlockDeltaDelta p `shouldBe` TextDelta "Hi"
        _ -> expectationFailure "Expected ContentBlockDelta"

    it "parses error event" $ do
      let json = "{\"type\":\"error\",\"error\":{\"message\":\"overloaded\"}}"
      decode json `shouldBe` Just (StreamError "overloaded")

    it "round-trips MessageStop"
      $ decode (encode MessageStop) `shouldBe` Just MessageStop

    it "round-trips Ping"
      $ decode (encode Ping) `shouldBe` Just Ping

    it "round-trips ContentBlockStop"
      $ decode (encode (ContentBlockStop 2)) `shouldBe` Just (ContentBlockStop 2)

    it "round-trips ContentBlockDelta"
      $ property
      $ \(p :: ContentBlockDeltaPayload) ->
        let evt = ContentBlockDelta p
         in decode (encode evt) === Just evt

    it "round-trips MessageDelta"
      $ property
      $ \(p :: MessageDeltaPayload) ->
        let evt = MessageDelta p
         in decode (encode evt) === Just evt
