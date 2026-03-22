{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Anthropic.Claude.Types.ResponseSpec (spec) where

import Anthropic.Claude.Types.Response
import Anthropic.Claude.Types.Core
import Anthropic.Claude.Types.Common
import Anthropic.Claude.Types.Client
import Data.Aeson (decode, encode)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

-- Arbitrary Instances

genText :: Gen T.Text
genText = T.pack <$> listOf1 (elements ['a'..'z'])

instance Arbitrary Usage where
  arbitrary = Usage
    <$> choose (0, 10000)
    <*> choose (0, 10000)

instance Arbitrary MessageResponse where
  arbitrary = MessageResponse
    <$> (MessageId <$> genText)
    <*> pure "message"
    <*> pure Assistant
    <*> listOf1 (TextBlock <$> genText)
    <*> (ModelId <$> genText)
    <*> (Just <$> elements [EndTurn, MaxTokens, StopSequence, ToolUse])
    <*> (Just <$> genText)
    <*> arbitrary

instance Arbitrary a => Arbitrary (APIResponse a) where
  arbitrary = APIResponse
    <$> arbitrary
    <*> pure Nothing
    <*> pure Nothing

-- Tests

spec :: Spec
spec = describe "Types.Response" $ do

  describe "Usage" $ do
    it "round-trips through JSON" $ property $
      \(usage :: Usage) -> decode (encode usage) === Just usage

    it "uses snake_case for field names" $ do
      let usage = Usage 100 200
      T.isInfixOf "input_tokens" (T.pack $ show $ encode usage) `shouldBe` True

  describe "MessageResponse" $ do
    it "round-trips through JSON" $ property $
      \(resp :: MessageResponse) -> decode (encode resp) === Just resp

    it "uses snake_case for field names" $ do
      let resp = MessageResponse
            (MessageId "msg_123")
            "message"
            Assistant
            [TextBlock "hello"]
            claude4Sonnet
            (Just EndTurn)
            Nothing
            (Usage 10 20)
      T.isInfixOf "stop_reason" (T.pack $ show $ encode resp) `shouldBe` True

  describe "APIResponse" $ do
    it "round-trips through JSON" $ property $
      \(resp :: APIResponse MessageResponse) -> decode (encode resp) === Just resp

  describe "Helper Functions" $ do
    it "extractText concatenates text blocks" $ do
      let resp = MessageResponse
            (MessageId "msg_123")
            "message"
            Assistant
            [TextBlock "Hello", TextBlock "World"]
            claude4Sonnet
            Nothing
            Nothing
            (Usage 10 20)
      extractText resp `shouldBe` "Hello\nWorld"

    it "extractText ignores non-text blocks" $ do
      let resp = MessageResponse
            (MessageId "msg_123")
            "message"
            Assistant
            [TextBlock "Hello", ImageBlock (URLSource "https://example.com/img.jpg"), TextBlock "World"]
            claude4Sonnet
            Nothing
            Nothing
            (Usage 10 20)
      extractText resp `shouldBe` "Hello\nWorld"
