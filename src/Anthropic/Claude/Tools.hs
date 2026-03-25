{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Anthropic.Claude.Tools
Description : Tool use helpers
Copyright   : (c) 2026 Geoffrey Noël
License     : MIT
Maintainer  : noel.geoff@gmail.com

Helper functions for working with Claude's tool use feature.
Provides smart constructors for defining tools, extracting
tool calls from responses, and building tool results.
-}
module Anthropic.Claude.Tools
  ( -- * Tool Definition
    defineTool

    -- * Tool Call Extraction
  , extractToolCalls

    -- * Tool Result Construction
  , buildToolResult
  , buildToolError
  ) where

import Anthropic.Claude.Types.ContentBlock
import Anthropic.Claude.Types.Core
import Anthropic.Claude.Types.Request (Tool(..))
import Anthropic.Claude.Types.Response (MessageResponse(..))
import Anthropic.Claude.Types.Schema
import Data.Aeson (Value(..))
import Data.Aeson.Text (encodeToLazyText)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL

-- | Define a tool with name, description, and properties.
--
-- Constructs a 'Tool' with @type: "custom"@, wrapping the properties
-- in an object schema.
--
-- Example:
-- @
-- let weatherTool = defineTool
--       "get_weather"
--       "Get the current weather for a location"
--       [ required "location"
--           (withDescription "City and state, e.g. San Francisco, CA" stringSchema)
--       ]
-- @
defineTool :: Text -> Text -> [Property] -> Tool
defineTool name desc props = Tool name (Just desc) (objectSchema props) Nothing

-- | Extract all tool use blocks from a message response.
--
-- Returns a list of (ToolCallId, tool name, input) tuples
-- for each tool call Claude wants to make.
--
-- Example:
-- @
-- let calls = extractToolCalls response
-- forM_ calls $ \\(toolId, name, input) -> do
--   result <- executeMyTool name input
--   let resultBlock = buildToolResult toolId result
--   ...
-- @
extractToolCalls :: MessageResponse -> [(ToolCallId, Text, Value)]
extractToolCalls resp =
  [ (blockToolUseId block, blockToolName block, Object (unToolUseInput (blockToolInput block)))
  | block <- responseContent resp
  , isToolUse block
  ]
  where
    isToolUse (ToolUseBlock _ _ _ _) = True
    isToolUse _ = False

-- | Build a tool result content block for a successful tool execution.
--
-- The Value is JSON-encoded to text for the API.
--
-- Example:
-- @
-- let result = buildToolResult toolCallId (object ["temperature" .= (72 :: Int)])
-- -- Produces: ToolResultText "{\"temperature\":72}"
-- @
buildToolResult :: ToolCallId -> Value -> ContentBlock
buildToolResult toolId result =
  toolResultText toolId (TL.toStrict (encodeToLazyText result)) (Just False)

-- | Build a tool result content block for a failed tool execution.
--
-- Example:
-- @
-- let err = buildToolError toolCallId "Location not found"
-- @
buildToolError :: ToolCallId -> Text -> ContentBlock
buildToolError toolId errMsg = toolResultText toolId errMsg (Just True)
