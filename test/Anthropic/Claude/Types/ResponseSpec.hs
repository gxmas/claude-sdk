{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Anthropic.Claude.Types.ResponseSpec (spec) where

import Anthropic.Claude.Types.ContentBlock
import Anthropic.Claude.Types.Core
import Anthropic.Claude.Types.Response
import Data.Aeson (decode, encode)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

-- Arbitrary Instances

genText :: Gen T.Text
genText = T.pack <$> listOf1 (elements ['a' .. 'z'])

instance Arbitrary Usage where
  arbitrary =
    Usage
      <$> choose (0, 10000)
      <*> choose (0, 10000)
      <*> oneof [pure Nothing, Just <$> choose (0, 10000)]
      <*> oneof [pure Nothing, Just <$> choose (0, 10000)]

instance Arbitrary MessageResponse where
  arbitrary =
    MessageResponse
      <$> (MessageId <$> genText)
      <*> pure "message"
      <*> pure Assistant
      <*> listOf1 (TextBlock <$> genText <*> pure Nothing)
      <*> (ModelId <$> genText)
      <*> (Just <$> elements [EndTurn, MaxTokens, StopSequence, ToolUse])
      <*> (Just <$> genText)
      <*> arbitrary

instance Arbitrary TokenCount where
  arbitrary = TokenCount <$> choose (0, 100000)

instance Arbitrary a => Arbitrary (APIResponse a) where
  arbitrary =
    APIResponse
      <$> arbitrary
      <*> pure Nothing
      <*> pure Nothing

-- Tests

spec :: Spec
spec = describe "Types.Response" $ do
  describe "Usage" $ do
    it "round-trips through JSON"
      $ property
      $ \(usage :: Usage) -> decode (encode usage) === Just usage

    it "uses snake_case for field names" $ do
      let usage = Usage 100 200 Nothing Nothing
      T.isInfixOf "input_tokens" (T.pack $ show $ encode usage) `shouldBe` True

  describe "MessageResponse" $ do
    it "round-trips through JSON"
      $ property
      $ \(resp :: MessageResponse) -> decode (encode resp) === Just resp

    it "uses snake_case for field names" $ do
      let resp =
            MessageResponse
              (MessageId "msg_123")
              "message"
              Assistant
              [TextBlock "hello" Nothing]
              claude4Sonnet
              (Just EndTurn)
              Nothing
              (Usage 10 20 Nothing Nothing)
      T.isInfixOf "stop_reason" (T.pack $ show $ encode resp) `shouldBe` True

  describe "APIResponse" $ do
    it "round-trips through JSON"
      $ property
      $ \(resp :: APIResponse MessageResponse) -> decode (encode resp) === Just resp

  describe "TokenCount" $ do
    it "parses from JSON" $ do
      let json = "{\"input_tokens\":42}"
      decode json `shouldBe` Just (TokenCount 42)

    it "encodes to JSON with snake_case" $ do
      let tc = TokenCount 100
          jsonText = T.pack $ show $ encode tc
      T.isInfixOf "input_tokens" jsonText `shouldBe` True

    it "round-trips through JSON"
      $ property
      $ \(tc :: TokenCount) -> decode (encode tc) === Just tc

  describe "Helper Functions" $ do
    it "extractText concatenates text blocks" $ do
      let resp =
            MessageResponse
              (MessageId "msg_123")
              "message"
              Assistant
              [TextBlock "Hello" Nothing, TextBlock "World" Nothing]
              claude4Sonnet
              Nothing
              Nothing
              (Usage 10 20 Nothing Nothing)
      extractText resp `shouldBe` "Hello\nWorld"

    it "extractText ignores non-text blocks" $ do
      let resp =
            MessageResponse
              (MessageId "msg_123")
              "message"
              Assistant
              [TextBlock "Hello" Nothing, ImageBlock (URLSource "https://example.com/img.jpg") Nothing, TextBlock "World" Nothing]
              claude4Sonnet
              Nothing
              Nothing
              (Usage 10 20 Nothing Nothing)
      extractText resp `shouldBe` "Hello\nWorld"

  describe "Usage cache fields" $ do
    it "round-trips with cache token counts" $ do
      let usage = Usage 100 200 (Just 50) (Just 30)
      decode (encode usage) `shouldBe` Just usage

    it "omits cache fields when Nothing" $ do
      let usage = Usage 100 200 Nothing Nothing
          jsonText = T.pack $ show $ encode usage
      T.isInfixOf "cache_creation_input_tokens" jsonText `shouldBe` False
      T.isInfixOf "cache_read_input_tokens" jsonText `shouldBe` False

    it "includes cache fields when present" $ do
      let usage = Usage 100 200 (Just 50) (Just 30)
          jsonText = T.pack $ show $ encode usage
      T.isInfixOf "cache_creation_input_tokens" jsonText `shouldBe` True
      T.isInfixOf "cache_read_input_tokens" jsonText `shouldBe` True
