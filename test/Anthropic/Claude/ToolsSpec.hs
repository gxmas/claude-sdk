{-# LANGUAGE OverloadedStrings #-}

module Anthropic.Claude.ToolsSpec (spec) where

import Anthropic.Claude.Tools
import Anthropic.Claude.Types.ContentBlock
import Anthropic.Claude.Types.Core
import Anthropic.Claude.Types.Request (Tool (..))
import Anthropic.Claude.Types.Response
import Anthropic.Claude.Types.Schema
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec = describe "Tools" $ do
  describe "defineTool" $ do
    it "creates a Tool with given name, description, and properties" $ do
      let tool =
            defineTool
              "get_weather"
              "Get weather for a location"
              [ required
                  "location"
                  (withDescription "City and state, e.g. San Francisco, CA" stringSchema)
              ]
      toolName tool `shouldBe` "get_weather"
      toolDescription tool `shouldBe` Just "Get weather for a location"
      schemaType (toolInputSchema tool) `shouldBe` Just (SingleType ObjectType)
      schemaRequired (toolInputSchema tool) `shouldBe` Just ["location"]
      toolCacheControl tool `shouldBe` Nothing

    it "wraps description in Just" $ do
      let tool = defineTool "test" "desc" []
      toolDescription tool `shouldBe` Just "desc"

    it "sets cache_control to Nothing" $ do
      let tool = defineTool "test" "desc" []
      toolCacheControl tool `shouldBe` Nothing

  describe "extractToolCalls" $ do
    it "extracts tool use blocks from response" $ do
      let toolId = ToolCallId "toolu_123"
          inputObj = KM.fromList [("location", String "SF")]
          inputValue = object ["location" .= ("SF" :: T.Text)]
          resp =
            MessageResponse
              (MessageId "msg_1")
              "message"
              Assistant
              [ TextBlock "I'll check the weather." Nothing Nothing
              , ToolUseBlock toolId "get_weather" (ToolUseInput inputObj) Nothing
              ]
              (ModelId "claude")
              (Just ToolUse)
              Nothing
              (Usage 10 20 Nothing Nothing)
          calls = extractToolCalls resp
      length calls `shouldBe` 1
      let (cid, name, inp) = head calls
      cid `shouldBe` toolId
      name `shouldBe` "get_weather"
      inp `shouldBe` inputValue

    it "extracts multiple tool calls" $ do
      let resp =
            MessageResponse
              (MessageId "msg_1")
              "message"
              Assistant
              [ ToolUseBlock (ToolCallId "t1") "tool_a" (ToolUseInput KM.empty) Nothing
              , TextBlock "between" Nothing Nothing
              , ToolUseBlock (ToolCallId "t2") "tool_b" (ToolUseInput KM.empty) Nothing
              ]
              (ModelId "claude")
              (Just ToolUse)
              Nothing
              (Usage 10 20 Nothing Nothing)
      length (extractToolCalls resp) `shouldBe` 2

    it "returns empty list when no tool calls" $ do
      let resp =
            MessageResponse
              (MessageId "msg_1")
              "message"
              Assistant
              [TextBlock "Just text" Nothing Nothing]
              (ModelId "claude")
              (Just EndTurn)
              Nothing
              (Usage 10 20 Nothing Nothing)
      extractToolCalls resp `shouldBe` []

  describe "buildToolResult" $ do
    it "creates ToolResultBlock with is_error=False" $ do
      let toolId = ToolCallId "toolu_123"
          result = object ["temperature" .= (72 :: Int)]
          block = buildToolResult toolId result
      -- buildToolResult converts Value to ToolResultContent
      case block of
        ToolResultBlock tid (ToolResultText _) isErr _ ->
          (tid, isErr) `shouldBe` (toolId, Just False)
        _ -> expectationFailure "Expected ToolResultBlock with ToolResultText"

  describe "buildToolError" $ do
    it "creates ToolResultBlock with is_error=True" $ do
      let toolId = ToolCallId "toolu_123"
          errMsg = "Location not found"
          block = buildToolError toolId errMsg
      block `shouldBe` ToolResultBlock toolId (ToolResultText "Location not found") (Just True) Nothing
