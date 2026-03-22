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

import Anthropic.Claude.Types.Common
import Anthropic.Claude.Types.Core
import Anthropic.Claude.Types.Request (Tool(..))
import Anthropic.Claude.Types.Response (MessageResponse(..))
import Anthropic.Claude.Types.Schema
import Data.Aeson (Value(..))
import Data.Text (Text)

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
  [ (blockToolUseId block, blockToolName block, blockToolInput block)
  | block <- responseContent resp
  , isToolUse block
  ]
  where
    isToolUse (ToolUseBlock _ _ _ _) = True
    isToolUse _ = False

-- | Build a tool result content block for a successful tool execution.
--
-- Example:
-- @
-- let result = buildToolResult toolCallId (object ["temperature" .= (72 :: Int)])
-- @
buildToolResult :: ToolCallId -> Value -> ContentBlock
buildToolResult toolId result = ToolResultBlock toolId result (Just False) Nothing

-- | Build a tool result content block for a failed tool execution.
--
-- Example:
-- @
-- let err = buildToolError toolCallId (String "Location not found")
-- @
buildToolError :: ToolCallId -> Value -> ContentBlock
buildToolError toolId errMsg = ToolResultBlock toolId errMsg (Just True) Nothing
