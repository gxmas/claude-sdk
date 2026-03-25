{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Anthropic.Claude.Types.CommonSpec (spec) where

import Anthropic.Claude.Types.Common
import Anthropic.Claude.Types.Core
import Data.Aeson (Value(..), decode, encode, object)
import Data.Aeson.Key (fromText)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

-- QuickCheck Generators

instance Arbitrary CacheControl where
  arbitrary = CacheControl <$> pure "ephemeral"

instance Arbitrary ImageSource where
  arbitrary = oneof
    [ Base64Source <$> genMediaType <*> genBase64
    , URLSource <$> genUrl
    ]
    where
      genMediaType = elements ["image/jpeg", "image/png", "image/webp", "image/gif"]
      genBase64 = T.pack <$> listOf1 (elements $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+', '/', '='])
      genUrl = ("https://example.com/image" <>) . T.pack . show <$> (arbitrary :: Gen Int)

instance Arbitrary ToolCallId where
  arbitrary = ToolCallId <$> genText
    where
      genText = T.pack <$> listOf1 (elements ['a'..'z'])

instance Arbitrary ToolResultContent where
  arbitrary = frequency
    [ (3, ToolResultText . T.pack <$> listOf1 (elements ['a'..'z']))
    , (1, ToolResultBlocks <$> resize 2 arbitrary)
    ]

instance Arbitrary ContentBlock where
  arbitrary = oneof
    [ TextBlock <$> genText <*> arbitrary
    , ImageBlock <$> arbitrary <*> arbitrary
    , ToolUseBlock <$> arbitrary <*> genText <*> genJsonValue <*> arbitrary
    , ToolResultBlock <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    ]
    where
      genText = T.pack <$> listOf1 (elements ['a'..'z'])
      genJsonValue = oneof
        [ String <$> genText
        , Number . fromIntegral <$> (arbitrary :: Gen Int)
        , Bool <$> arbitrary
        , pure Null
        , object <$> listOf ((,) <$> (fromText <$> genText) <*> (String <$> genText))
        ]

instance Arbitrary MessageContent where
  arbitrary = oneof
    [ TextContent <$> (T.pack <$> listOf1 (elements ['a'..'z']))
    , BlocksContent <$> listOf1 arbitrary
    ]

-- Test Suite

spec :: Spec
spec = describe "Types.Common" $ do

  describe "CacheControl" $ do
    it "round-trips through JSON" $ property $
      \(cc :: CacheControl) -> decode (encode cc) === Just cc

    it "parses cache control with type field" $ do
      let json = "{\"type\":\"ephemeral\"}"
      decode json `shouldBe` Just (CacheControl "ephemeral")

  describe "ImageSource" $ do
    it "parses Base64Source from JSON" $ do
      let json = "{\"type\":\"base64\",\"media_type\":\"image/jpeg\",\"data\":\"iVBORw0KGg...\"}"
      let expected = Base64Source "image/jpeg" "iVBORw0KGg..."
      decode json `shouldBe` Just expected

    it "parses URLSource from JSON" $ do
      let json = "{\"type\":\"url\",\"url\":\"https://example.com/image.jpg\"}"
      let expected = URLSource "https://example.com/image.jpg"
      decode json `shouldBe` Just expected

    it "encodes Base64Source with type field" $ do
      let src = Base64Source "image/png" "abc123"
      let json = encode src
      decode json `shouldBe` Just src

    it "encodes URLSource with type field" $ do
      let src = URLSource "https://example.com/test.png"
      let json = encode src
      decode json `shouldBe` Just src

    it "round-trips through JSON" $ property $
      \(src :: ImageSource) -> decode (encode src) === Just src

    it "uses snake_case for media_type field" $ do
      let src = Base64Source "image/jpeg" "data"
      T.isInfixOf "media_type" (T.pack $ show $ encode src) `shouldBe` True

  describe "ContentBlock" $ do
    it "parses TextBlock from JSON" $ do
      let json = "{\"type\":\"text\",\"text\":\"Hello, world!\"}"
      let expected = TextBlock "Hello, world!" Nothing
      decode json `shouldBe` Just expected

    it "parses ImageBlock from JSON" $ do
      let json = "{\"type\":\"image\",\"source\":{\"type\":\"base64\",\"media_type\":\"image/jpeg\",\"data\":\"abc\"}}"
      let expected = ImageBlock (Base64Source "image/jpeg" "abc") Nothing
      decode json `shouldBe` Just expected

    it "parses ToolUseBlock from JSON" $ do
      let json = "{\"type\":\"tool_use\",\"id\":\"toolu_123\",\"name\":\"get_weather\",\"input\":{\"location\":\"SF\"}}"
      let toolId = ToolCallId "toolu_123"
      case decode json of
        Just (ToolUseBlock tid name _ _) -> do
          tid `shouldBe` toolId
          name `shouldBe` "get_weather"
        _ -> expectationFailure "Failed to parse ToolUseBlock"

    it "parses ToolResultBlock from JSON (text content)" $ do
      let json = "{\"type\":\"tool_result\",\"tool_use_id\":\"toolu_123\",\"content\":\"success\",\"is_error\":false}"
      case decode json of
        Just (ToolResultBlock _ (ToolResultText "success") isErr _) -> isErr `shouldBe` Just False
        _ -> expectationFailure "Failed to parse ToolResultBlock with text content"

    it "parses ToolResultBlock from JSON (blocks content)" $ do
      let json = "{\"type\":\"tool_result\",\"tool_use_id\":\"toolu_123\",\"content\":[{\"type\":\"text\",\"text\":\"result\"}],\"is_error\":false}"
      case decode json of
        Just (ToolResultBlock _ (ToolResultBlocks _) isErr _) -> isErr `shouldBe` Just False
        _ -> expectationFailure "Failed to parse ToolResultBlock with blocks content"

    it "encodes TextBlock with correct type field" $ do
      let block = TextBlock "test" Nothing
      case decode (encode block) of
        Just (TextBlock _ _) -> pure ()
        _ -> expectationFailure "Failed to roundtrip TextBlock"

    it "encodes ImageBlock with correct type field" $ do
      let block = ImageBlock (URLSource "https://example.com/img.jpg") Nothing
      case decode (encode block) of
        Just (ImageBlock _ _) -> pure ()
        _ -> expectationFailure "Failed to roundtrip ImageBlock"

    it "encodes ToolUseBlock with correct type field" $ do
      let block = ToolUseBlock (ToolCallId "id") "tool" Null Nothing
      case decode (encode block) of
        Just (ToolUseBlock _ _ _ _) -> pure ()
        _ -> expectationFailure "Failed to roundtrip ToolUseBlock"

    it "encodes ToolResultBlock with correct type field" $ do
      let block = ToolResultBlock (ToolCallId "id") (ToolResultText "test") Nothing Nothing
      case decode (encode block) of
        Just (ToolResultBlock _ _ _ _) -> pure ()
        _ -> expectationFailure "Failed to roundtrip ToolResultBlock"

    it "round-trips through JSON" $ property $
      \(block :: ContentBlock) -> decode (encode block) === Just block

  describe "MessageContent" $ do
    it "parses text content as TextContent" $ do
      let json = "\"Hello, world!\""
      decode json `shouldBe` Just (TextContent "Hello, world!")

    it "parses array as BlocksContent" $ do
      let json = "[{\"type\":\"text\",\"text\":\"Hello\"}]"
      case decode json of
        Just (BlocksContent [TextBlock "Hello" Nothing]) -> pure ()
        _ -> expectationFailure "Failed to parse BlocksContent"

    it "encodes TextContent as plain string" $ do
      let content = TextContent "test"
      encode content `shouldBe` "\"test\""

    it "encodes BlocksContent as array" $ do
      let content = BlocksContent [TextBlock "hello" Nothing]
      case decode (encode content) of
        Just (BlocksContent _) -> pure ()
        _ -> expectationFailure "Failed to encode BlocksContent as array"

    it "round-trips through JSON" $ property $
      \(content :: MessageContent) -> decode (encode content) === Just content

  describe "Helper Constructors" $ do
    it "textBlock creates TextBlock" $ do
      textBlock "hello" `shouldBe` TextBlock "hello" Nothing

    it "imageBlock creates ImageBlock with Base64Source" $ do
      let block = imageBlock "image/png" "data123"
      block `shouldBe` ImageBlock (Base64Source "image/png" "data123") Nothing

    it "toolUseBlock creates ToolUseBlock" $ do
      let block = toolUseBlock (ToolCallId "id") "tool" Null
      block `shouldBe` ToolUseBlock (ToolCallId "id") "tool" Null Nothing

    it "toolResultText creates ToolResultBlock with text content" $ do
      let block = toolResultText (ToolCallId "id") "success" Nothing
      block `shouldBe` ToolResultBlock (ToolCallId "id") (ToolResultText "success") Nothing Nothing

    it "toolResultBlocks creates ToolResultBlock with blocks content" $ do
      let block = toolResultBlocks (ToolCallId "id") [textBlock "result"] (Just False)
      block `shouldBe` ToolResultBlock (ToolCallId "id") (ToolResultBlocks [textBlock "result"]) (Just False) Nothing

  describe "Cache Control on ContentBlock" $ do
    it "omits cache_control when Nothing" $ do
      let block = TextBlock "test" Nothing
          jsonText = T.pack $ show $ encode block
      T.isInfixOf "cache_control" jsonText `shouldBe` False

    it "includes cache_control when Just" $ do
      let block = TextBlock "test" (Just ephemeralCacheControl)
          jsonText = T.pack $ show $ encode block
      T.isInfixOf "cache_control" jsonText `shouldBe` True

    it "round-trips ContentBlock with cache_control" $ do
      let block = TextBlock "cached text" (Just ephemeralCacheControl)
      decode (encode block) `shouldBe` Just block

    it "withCacheControl sets cache_control on any block" $ do
      let block = withCacheControl ephemeralCacheControl (textBlock "hello")
      blockCacheControl block `shouldBe` Just ephemeralCacheControl

    it "ephemeralCacheControl has type ephemeral" $ do
      cacheType ephemeralCacheControl `shouldBe` "ephemeral"
