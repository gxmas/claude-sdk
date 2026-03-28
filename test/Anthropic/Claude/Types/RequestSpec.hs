{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Anthropic.Claude.Types.RequestSpec (spec) where

import Anthropic.Claude.Types.ContentBlock (CacheControl (..), ContentBlock (..), textBlock)
import Anthropic.Claude.Types.Core
import Anthropic.Claude.Types.Request
import Anthropic.Claude.Types.Schema
import Data.Aeson (decode, encode, object, (.=))
import Data.Function ((&))
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

-- Arbitrary Instances

genText :: Gen T.Text
genText = T.pack <$> listOf1 (elements ['a' .. 'z'])

instance Arbitrary MessageContent where
  arbitrary = TextContent <$> genText

instance Arbitrary Message where
  arbitrary =
    Message
      <$> elements [User, Assistant]
      <*> arbitrary

instance Arbitrary SchemaType where
  arbitrary =
    elements
      [StringType, NumberType, IntegerType, BooleanType, NullType]

instance Arbitrary JsonSchema where
  arbitrary =
    oneof
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
  arbitrary =
    Tool
      <$> genText
      <*> oneof [Just <$> genText, pure Nothing]
      <*> arbitrary
      <*> oneof [Just <$> arbitrary, pure Nothing]

instance Arbitrary ToolChoice where
  arbitrary =
    oneof
      [ pure AutoChoice
      , pure AnyChoice
      , ToolChoice <$> genText
      ]

instance Arbitrary ThinkingConfig where
  arbitrary =
    oneof
      [ ThinkingEnabled <$> choose (1, 100000)
      , pure ThinkingDisabled
      ]

instance Arbitrary SystemBlock where
  arbitrary =
    SystemBlock
      <$> genText
      <*> oneof [pure Nothing, Just <$> arbitrary]

instance Arbitrary SystemContent where
  arbitrary =
    oneof
      [ SystemText <$> genText
      , SystemBlocks <$> listOf1 arbitrary
      ]

instance Arbitrary CreateMessageRequest where
  arbitrary = do
    model <- ModelId <$> genText
    msgs <- listOf1 arbitrary
    maxTok <- choose (1, 4096)
    sys <- oneof [pure Nothing, Just <$> arbitrary]
    thinking <- oneof [pure Nothing, Just <$> arbitrary]
    serviceTier <- oneof [pure Nothing, Just <$> elements ["standard", "priority"]]
    let req = mkRequest model msgs maxTok
    pure $ req {requestSystem = sys, requestThinking = thinking, requestServiceTier = serviceTier}

-- Tests

spec :: Spec
spec = describe "Types.Request" $ do
  describe "MessageContent" $ do
    it "parses text content as TextContent" $ do
      let json = "\"Hello, world!\""
      decode json `shouldBe` Just (TextContent "Hello, world!")

    it "parses array as BlocksContent" $ do
      let json = "[{\"type\":\"text\",\"text\":\"Hello\"}]"
      case decode json of
        Just (BlocksContent [TextBlock "Hello" Nothing Nothing]) -> pure ()
        _ -> expectationFailure "Failed to parse BlocksContent"

    it "encodes TextContent as plain string" $ do
      let content = TextContent "test"
      encode content `shouldBe` "\"test\""

    it "encodes BlocksContent as array" $ do
      let content = BlocksContent [textBlock "hello"]
      case decode (encode content) of
        Just (BlocksContent _) -> pure ()
        _ -> expectationFailure "Failed to encode BlocksContent as array"

    it "round-trips through JSON"
      $ property
      $ \(content :: MessageContent) -> decode (encode content) === Just content

  describe "Message" $ do
    it "round-trips through JSON"
      $ property
      $ \(msg :: Message) -> decode (encode msg) === Just msg

  describe "Tool" $ do
    it "round-trips through JSON"
      $ property
      $ \(tool :: Tool) -> decode (encode tool) === Just tool

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
        Just t -> toolName t `shouldBe` "test"
        Nothing -> expectationFailure "Failed to parse tool without type field"

    it "rejects non-custom type" $ do
      let json = "{\"type\":\"server\",\"name\":\"test\",\"input_schema\":{}}"
      (decode json :: Maybe Tool) `shouldBe` Nothing

  describe "ToolChoice" $ do
    it "encodes AutoChoice correctly"
      $ encode AutoChoice `shouldBe` "{\"type\":\"auto\"}"

    it "encodes AnyChoice correctly"
      $ encode AnyChoice `shouldBe` "{\"type\":\"any\"}"

    it "encodes ToolChoice correctly"
      $ encode (ToolChoice "my_tool") `shouldBe` "{\"name\":\"my_tool\",\"type\":\"tool\"}"

    it "round-trips through JSON"
      $ property
      $ \(tc :: ToolChoice) -> decode (encode tc) === Just tc

  describe "ThinkingConfig" $ do
    it "encodes ThinkingEnabled correctly" $ do
      let json = "{\"budget_tokens\":10000,\"type\":\"enabled\"}"
      decode json `shouldBe` Just (ThinkingEnabled 10000)

    it "encodes ThinkingDisabled correctly" $ do
      let json = "{\"type\":\"disabled\"}"
      decode json `shouldBe` Just ThinkingDisabled

    it "round-trips through JSON"
      $ property
      $ \(tc :: ThinkingConfig) -> decode (encode tc) === Just tc

  describe "CreateMessageRequest" $ do
    it "round-trips through JSON"
      $ property
      $ \(req :: CreateMessageRequest) -> decode (encode req) === Just req

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

  describe "SystemContent" $ do
    it "round-trips through JSON"
      $ property
      $ \(sc :: SystemContent) -> decode (encode sc) === Just sc

    it "parses plain string as SystemText" $ do
      let json = "\"You are a helpful assistant.\""
      decode json `shouldBe` Just (SystemText "You are a helpful assistant.")

    it "parses array as SystemBlocks" $ do
      let json = "[{\"type\":\"text\",\"text\":\"Be helpful.\"}]"
      case decode json of
        Just (SystemBlocks [SystemBlock txt Nothing]) ->
          txt `shouldBe` "Be helpful."
        _ -> expectationFailure "Failed to parse SystemBlocks"

    it "encodes SystemText as plain string" $ do
      encode (SystemText "test") `shouldBe` "\"test\""

  describe "SystemBlock" $ do
    it "round-trips through JSON"
      $ property
      $ \(sb :: SystemBlock) -> decode (encode sb) === Just sb

    it "always emits type field in JSON" $ do
      let sb = systemBlock "hello"
          jsonText = T.pack $ show $ encode sb
      -- show of ByteString includes escaped quotes, so check for the key
      T.isInfixOf "type" jsonText `shouldBe` True

    it "omits cache_control when Nothing" $ do
      let sb = systemBlock "hello"
          jsonText = T.pack $ show $ encode sb
      T.isInfixOf "cache_control" jsonText `shouldBe` False

    it "includes cache_control when present" $ do
      let sb = cachedSystemBlock "hello"
          jsonText = T.pack $ show $ encode sb
      T.isInfixOf "cache_control" jsonText `shouldBe` True

    it "cachedSystemBlock uses ephemeral cache control" $ do
      systemBlockCacheControl (cachedSystemBlock "test") `shouldBe` Just (CacheControl "ephemeral")

  describe "Request Builder Combinators" $ do
    let base = mkRequest claude4Sonnet [userMsg "test"] 1024

    it "withMetadata sets metadata" $ do
      let meta = object ["user_id" .= ("u123" :: T.Text)]
          req = withMetadata meta base
      requestMetadata req `shouldBe` Just meta

    it "withStopSequences sets stop sequences" $ do
      let req = withStopSequences ["stop", "end"] base
      requestStopSequences req `shouldBe` Just ["stop", "end"]

    it "withStreaming enables streaming" $ do
      let req = withStreaming base
      requestStream req `shouldBe` Just True

    it "withSystem sets system prompt" $ do
      let req = withSystem (SystemText "Be helpful.") base
      requestSystem req `shouldBe` Just (SystemText "Be helpful.")

    it "withTemperature sets temperature" $ do
      let req = withTemperature 0.7 base
      requestTemperature req `shouldBe` Just 0.7

    it "withToolChoice sets tool choice" $ do
      let req = withToolChoice AutoChoice base
      requestToolChoice req `shouldBe` Just AutoChoice

    it "withTools sets tools" $ do
      let tool = Tool "test" (Just "A test") stringSchema Nothing
          req = withTools [tool] base
      requestTools req `shouldBe` Just [tool]

    it "withTopK sets top-k" $ do
      let req = withTopK 40 base
      requestTopK req `shouldBe` Just 40

    it "withTopP sets top-p" $ do
      let req = withTopP 0.9 base
      requestTopP req `shouldBe` Just 0.9

    it "withThinking sets thinking config" $ do
      let req = withThinking (ThinkingEnabled 10000) base
      requestThinking req `shouldBe` Just (ThinkingEnabled 10000)

    it "withServiceTier sets service tier" $ do
      let req = withServiceTier "priority" base
      requestServiceTier req `shouldBe` Just "priority"

    it "combinators compose via (&)" $ do
      let req = base
              & withSystem (SystemText "Be helpful.")
              & withTemperature 0.7
              & withStreaming
      requestSystem req `shouldBe` Just (SystemText "Be helpful.")
      requestTemperature req `shouldBe` Just 0.7
      requestStream req `shouldBe` Just True

    it "later combinator overrides earlier" $ do
      let req = base
              & withTemperature 0.5
              & withTemperature 0.9
      requestTemperature req `shouldBe` Just 0.9

    it "preserves base request fields" $ do
      let req = base & withTemperature 0.7
      requestModel req `shouldBe` claude4Sonnet
      requestMessages req `shouldBe` [userMsg "test"]
      requestMaxTokens req `shouldBe` 1024
