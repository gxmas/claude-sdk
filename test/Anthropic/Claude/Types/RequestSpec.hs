{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Anthropic.Claude.Types.RequestSpec (spec) where

import Anthropic.Claude.Types.Request
import Anthropic.Claude.Types.Core
import Anthropic.Claude.Types.Common
import Anthropic.Claude.Types.Schema
import Data.Aeson (Value(..), decode, encode, object)
import Data.Aeson.Key (fromText)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

-- Arbitrary Instances

genText :: Gen T.Text
genText = T.pack <$> listOf1 (elements ['a'..'z'])

instance Arbitrary Message where
  arbitrary = Message
    <$> elements [User, Assistant]
    <*> (TextContent <$> genText)

instance Arbitrary SchemaType where
  arbitrary = elements
    [StringType, NumberType, IntegerType, BooleanType, NullType]

instance Arbitrary JsonSchema where
  arbitrary = oneof
    [ pure stringSchema
    , pure numberSchema
    , pure integerSchema
    , pure booleanSchema
    , objectSchema <$> listOf1 genProperty
    ]
    where
      genProperty = do
        name <- genText
        isReq <- arbitrary
        pure $ Property name stringSchema isReq

instance Arbitrary CacheControl where
  arbitrary = pure $ CacheControl "ephemeral"

instance Arbitrary Tool where
  arbitrary = Tool
    <$> genText
    <*> oneof [Just <$> genText, pure Nothing]
    <*> arbitrary
    <*> oneof [Just <$> arbitrary, pure Nothing]

instance Arbitrary ToolChoice where
  arbitrary = oneof
    [ pure AutoChoice
    , pure AnyChoice
    , ToolChoice <$> genText
    ]

instance Arbitrary CreateMessageRequest where
  arbitrary = do
    model <- ModelId <$> genText
    msgs <- listOf1 arbitrary
    maxTok <- choose (1, 4096)
    pure $ mkRequest model msgs maxTok

-- Tests

spec :: Spec
spec = describe "Types.Request" $ do

  describe "Message" $ do
    it "round-trips through JSON" $ property $
      \(msg :: Message) -> decode (encode msg) === Just msg

  describe "Tool" $ do
    it "round-trips through JSON" $ property $
      \(tool :: Tool) -> decode (encode tool) === Just tool

    it "serializes type as custom" $ do
      let tool = Tool "test" (Just "A test tool") stringSchema Nothing
          jsonText = T.pack $ show $ encode tool
      T.isInfixOf "custom" jsonText `shouldBe` True

    it "uses snake_case for field names" $ do
      let tool = Tool "test" (Just "A test tool") stringSchema Nothing
          jsonText = T.pack $ show $ encode tool
      T.isInfixOf "input_schema" jsonText `shouldBe` True

    it "omits description when Nothing" $ do
      let tool = Tool "test" Nothing stringSchema Nothing
          jsonText = T.pack $ show $ encode tool
      T.isInfixOf "description" jsonText `shouldBe` False

    it "omits cache_control when Nothing" $ do
      let tool = Tool "test" (Just "desc") stringSchema Nothing
          jsonText = T.pack $ show $ encode tool
      T.isInfixOf "cache_control" jsonText `shouldBe` False

    it "includes cache_control when present" $ do
      let tool = Tool "test" (Just "desc") stringSchema (Just (CacheControl "ephemeral"))
          jsonText = T.pack $ show $ encode tool
      T.isInfixOf "cache_control" jsonText `shouldBe` True

    it "parses tool without type field" $ do
      let json = "{\"name\":\"test\",\"input_schema\":{\"type\":\"string\"}}"
      case decode json :: Maybe Tool of
        Just t  -> toolName t `shouldBe` "test"
        Nothing -> expectationFailure "Failed to parse tool without type field"

    it "rejects non-custom type" $ do
      let json = "{\"type\":\"server\",\"name\":\"test\",\"input_schema\":{}}"
      (decode json :: Maybe Tool) `shouldBe` Nothing

  describe "ToolChoice" $ do
    it "encodes AutoChoice correctly" $
      encode AutoChoice `shouldBe` "{\"type\":\"auto\"}"

    it "encodes AnyChoice correctly" $
      encode AnyChoice `shouldBe` "{\"type\":\"any\"}"

    it "encodes ToolChoice correctly" $
      encode (ToolChoice "my_tool") `shouldBe` "{\"name\":\"my_tool\",\"type\":\"tool\"}"

    it "round-trips through JSON" $ property $
      \(tc :: ToolChoice) -> decode (encode tc) === Just tc

  describe "CreateMessageRequest" $ do
    it "round-trips through JSON" $ property $
      \(req :: CreateMessageRequest) -> decode (encode req) === Just req

    it "validates non-empty messages" $ do
      let json = "{\"model\":\"claude-opus-4-6\",\"messages\":[],\"max_tokens\":100}"
      (decode json :: Maybe CreateMessageRequest) `shouldBe` Nothing

    it "validates positive max_tokens" $ do
      let json = "{\"model\":\"claude-opus-4-6\",\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}],\"max_tokens\":0}"
      (decode json :: Maybe CreateMessageRequest) `shouldBe` Nothing

    it "uses snake_case for field names" $ do
      let req = mkRequest claude4Sonnet [userMsg "test"] 100
      let encoded = encode req
      T.isInfixOf "max_tokens" (T.pack $ show encoded) `shouldBe` True

  describe "Helper Constructors" $ do
    it "userMsg creates user message" $ do
      let msg = userMsg "Hello"
      messageRole msg `shouldBe` User

    it "assistantMsg creates assistant message" $ do
      let msg = assistantMsg "Hello"
      messageRole msg `shouldBe` Assistant

    it "mkRequest creates valid request" $ do
      let req = mkRequest claude4Sonnet [userMsg "test"] 1024
      requestModel req `shouldBe` claude4Sonnet
      length (requestMessages req) `shouldBe` 1
      requestMaxTokens req `shouldBe` 1024
