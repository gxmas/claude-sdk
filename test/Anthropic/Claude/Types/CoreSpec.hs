{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Anthropic.Claude.Types.CoreSpec (spec) where

import Anthropic.Claude.Types.Core
import Data.Aeson (decode, encode)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

-- QuickCheck Generators

instance Arbitrary ApiKey where
  arbitrary = ApiKey <$> genText

instance Arbitrary ModelId where
  arbitrary = ModelId <$> genText

instance Arbitrary RequestId where
  arbitrary = RequestId <$> genText

instance Arbitrary MessageId where
  arbitrary = MessageId <$> genText

instance Arbitrary BatchId where
  arbitrary = BatchId <$> genText

instance Arbitrary ToolCallId where
  arbitrary = ToolCallId <$> genText

instance Arbitrary Role where
  arbitrary = elements [User, Assistant]

instance Arbitrary StopReason where
  arbitrary = elements [EndTurn, MaxTokens, StopSequence, ToolUse]

-- Helper to generate random Text
genText :: Gen Text
genText = T.pack <$> listOf1 (elements ['a'..'z'])

-- Test Suite

spec :: Spec
spec = describe "Types.Core" $ do

  describe "ApiKey" $ do
    it "round-trips through JSON" $ property $
      \(key :: ApiKey) -> decode (encode key) === Just key

  describe "ModelId" $ do
    it "round-trips through JSON" $ property $
      \(modelId :: ModelId) -> decode (encode modelId) === Just modelId

  describe "RequestId" $ do
    it "round-trips through JSON" $ property $
      \(reqId :: RequestId) -> decode (encode reqId) === Just reqId

  describe "MessageId" $ do
    it "round-trips through JSON" $ property $
      \(msgId :: MessageId) -> decode (encode msgId) === Just msgId

  describe "BatchId" $ do
    it "round-trips through JSON" $ property $
      \(batchId :: BatchId) -> decode (encode batchId) === Just batchId

  describe "ToolCallId" $ do
    it "round-trips through JSON" $ property $
      \(toolId :: ToolCallId) -> decode (encode toolId) === Just toolId

  describe "Role" $ do
    it "encodes User as \"user\"" $
      encode User `shouldBe` "\"user\""

    it "encodes Assistant as \"assistant\"" $
      encode Assistant `shouldBe` "\"assistant\""

    it "decodes \"user\" as User" $
      decode "\"user\"" `shouldBe` Just User

    it "decodes \"assistant\" as Assistant" $
      decode "\"assistant\"" `shouldBe` Just Assistant

    it "round-trips through JSON" $ property $
      \(role :: Role) -> decode (encode role) === Just role

  describe "StopReason" $ do
    it "encodes EndTurn as \"end_turn\"" $
      encode EndTurn `shouldBe` "\"end_turn\""

    it "encodes MaxTokens as \"max_tokens\"" $
      encode MaxTokens `shouldBe` "\"max_tokens\""

    it "encodes StopSequence as \"stop_sequence\"" $
      encode StopSequence `shouldBe` "\"stop_sequence\""

    it "encodes ToolUse as \"tool_use\"" $
      encode ToolUse `shouldBe` "\"tool_use\""

    it "decodes \"end_turn\" as EndTurn" $
      decode "\"end_turn\"" `shouldBe` Just EndTurn

    it "decodes \"max_tokens\" as MaxTokens" $
      decode "\"max_tokens\"" `shouldBe` Just MaxTokens

    it "decodes \"stop_sequence\" as StopSequence" $
      decode "\"stop_sequence\"" `shouldBe` Just StopSequence

    it "decodes \"tool_use\" as ToolUse" $
      decode "\"tool_use\"" `shouldBe` Just ToolUse

    it "round-trips through JSON" $ property $
      \(stopReason :: StopReason) -> decode (encode stopReason) === Just stopReason

  describe "Model Constants" $ do
    it "claude4Opus is \"claude-opus-4-6\"" $
      claude4Opus `shouldBe` ModelId "claude-opus-4-6"

    it "claude4Sonnet is \"claude-sonnet-4-6\"" $
      claude4Sonnet `shouldBe` ModelId "claude-sonnet-4-6"

    it "claude35Haiku is \"claude-haiku-4-5-20251001\"" $
      claude35Haiku `shouldBe` ModelId "claude-haiku-4-5-20251001"
