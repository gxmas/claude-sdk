{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Anthropic.Claude.Types.ContentBlock
Description : Content block types and their dependencies
Copyright   : (c) 2026 Geoffrey Noël
License     : MIT
Maintainer  : noel.geoff@gmail.com

Content block types for Claude API messages. A 'ContentBlock' represents
a single piece of content in a message (text, image, tool use, or tool result).

This module contains:

* 'ContentBlock' and its 5 constructors (TextBlock, ImageBlock, ToolUseBlock, ToolResultBlock, ThinkingBlock)
* Direct dependencies of ContentBlock (CacheControl, ImageSource, ToolResultContent, ToolUseInput)
* Helper constructors and combinators
-}
module Anthropic.Claude.Types.ContentBlock
  ( -- * Content Types
    ContentBlock (..)
  , ImageSource (..)
  , CacheControl (..)
  , ToolResultContent (..)
  , ToolUseInput (..)

    -- * Helper Constructors
  , textBlock
  , imageBlock
  , toolUseBlock
  , toolResultText
  , toolResultBlocks
  , thinkingBlock

    -- * Cache Control Helpers
  , withCacheControl
  , ephemeralCacheControl
  )
where

import Anthropic.Claude.Internal.JSON
import Anthropic.Claude.Types.Core
import Control.Applicative ((<|>))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Cache control configuration for prompt caching
--
-- Allows marking content blocks as cacheable to reduce latency and cost
-- on repeated requests. See: https://docs.anthropic.com/en/docs/prompt-caching
newtype CacheControl = CacheControl
  { cacheType :: Text
  -- ^ Currently only "ephemeral" is supported
  }
  deriving (Eq, Show, Generic)

instance FromJSON CacheControl where
  parseJSON = genericParseJSON (prefixOptions "cache")

instance ToJSON CacheControl where
  toJSON = genericToJSON (prefixOptions "cache")
  toEncoding = genericToEncoding (prefixOptions "cache")

-- | Image source for vision capabilities
--
-- Images can be provided as base64-encoded data or URLs
data ImageSource
  = Base64Source
      { base64MediaType :: Text
      -- ^ MIME type (e.g., "image/jpeg", "image/png")
      , base64Data :: Text
      -- ^ Base64-encoded image data
      }
  | URLSource
      { imageUrl :: Text
      -- ^ URL to image (must be accessible)
      }
  deriving (Eq, Show, Generic)

instance FromJSON ImageSource where
  parseJSON = withDiscriminator "type" $ \typeField obj -> case typeField of
    "base64" ->
      Base64Source
        <$> obj .: "media_type"
        <*> obj .: "data"
    "url" ->
      URLSource
        <$> obj .: "url"
    other -> fail $ "Unknown ImageSource type: " <> T.unpack other

instance ToJSON ImageSource where
  toJSON (Base64Source mediaType base64) =
    object
      [ "type" .= ("base64" :: Text)
      , "media_type" .= mediaType
      , "data" .= base64
      ]
  toJSON (URLSource url) =
    object
      [ "type" .= ("url" :: Text)
      , "url" .= url
      ]

-- | Tool result content
--
-- Per the Anthropic API spec, tool_result content must be either:
-- * A text string
-- * An array of content blocks (text, image, etc.)
--
-- This type enforces API compliance at compile time, preventing invalid
-- structures like Number, Bool, Null, or arbitrary Objects.
data ToolResultContent
  = -- | Simple text result
    ToolResultText Text
  | -- | Rich content with multiple blocks
    ToolResultBlocks [ContentBlock]
  deriving (Eq, Show, Generic)

instance FromJSON ToolResultContent where
  parseJSON v =
    (ToolResultText <$> parseJSON v)
      <|> (ToolResultBlocks <$> parseJSON v)

instance ToJSON ToolResultContent where
  toJSON (ToolResultText t) = toJSON t
  toJSON (ToolResultBlocks blocks) = toJSON blocks

-- | Tool use input parameters
--
-- Per the Anthropic API spec, tool_use input must always be a JSON object
-- containing the parameters to pass to the tool. Tool schemas always have
-- type: "object" in their input_schema.
--
-- This type enforces API compliance at compile time, preventing invalid
-- structures like String, Number, Bool, Null, or Array at the top level.
newtype ToolUseInput = ToolUseInput
  { unToolUseInput :: Object
  -- ^ JSON object containing tool parameters
  }
  deriving (Eq, Show, Generic)

instance FromJSON ToolUseInput where
  parseJSON = withObject "ToolUseInput" $ \o -> pure (ToolUseInput o)

instance ToJSON ToolUseInput where
  toJSON (ToolUseInput o) = Object o

-- | Content block in a message
--
-- Messages consist of one or more content blocks. This discriminated union
-- represents all possible block types:
--
-- * 'TextBlock' - Plain text content
-- * 'ImageBlock' - Image for vision capabilities
-- * 'ToolUseBlock' - Request to use a tool (in assistant messages)
-- * 'ToolResultBlock' - Tool execution result (in user messages)
-- * 'ThinkingBlock' - Chain-of-thought reasoning (when extended thinking is enabled)
data ContentBlock
  = TextBlock
      { blockText :: Text
      -- ^ Text content
      , blockCacheControl :: Maybe CacheControl
      -- ^ Cache control for prompt caching
      }
  | ImageBlock
      { blockSource :: ImageSource
      -- ^ Image source (base64 or URL)
      , blockCacheControl :: Maybe CacheControl
      -- ^ Cache control for prompt caching
      }
  | ToolUseBlock
      { blockToolUseId :: ToolCallId
      -- ^ Unique identifier for this tool use
      , blockToolName :: Text
      -- ^ Name of the tool to call
      , blockToolInput :: ToolUseInput
      -- ^ JSON object containing tool parameters
      , blockCacheControl :: Maybe CacheControl
      -- ^ Cache control for prompt caching
      }
  | ToolResultBlock
      { blockToolResultId :: ToolCallId
      -- ^ ID matching the ToolUseBlock
      , blockToolResult :: ToolResultContent
      -- ^ Tool execution result
      , blockIsError :: Maybe Bool
      -- ^ Whether the tool call resulted in an error
      , blockCacheControl :: Maybe CacheControl
      -- ^ Cache control for prompt caching
      }
  | ThinkingBlock
      { blockThinking :: Text
      -- ^ Thinking text (may be empty if redacted)
      , blockSignature :: Text
      -- ^ Opaque verification signature
      , blockCacheControl :: Maybe CacheControl
      -- ^ Cache control for prompt caching
      }
  deriving (Eq, Show, Generic)

instance FromJSON ContentBlock where
  parseJSON = withDiscriminator "type" $ \typeField obj -> case typeField of
    "text" ->
      TextBlock
        <$> obj .: "text"
        <*> obj .:? "cache_control"
    "image" ->
      ImageBlock
        <$> obj .: "source"
        <*> obj .:? "cache_control"
    "tool_use" ->
      ToolUseBlock
        <$> obj .: "id"
        <*> obj .: "name"
        <*> obj .: "input"
        <*> obj .:? "cache_control"
    "tool_result" ->
      ToolResultBlock
        <$> obj .: "tool_use_id"
        <*> obj .: "content"
        <*> obj .:? "is_error"
        <*> obj .:? "cache_control"
    "thinking" ->
      ThinkingBlock
        <$> obj .: "thinking"
        <*> obj .: "signature"
        <*> obj .:? "cache_control"
    other -> fail $ "Unknown ContentBlock type: " <> T.unpack other

instance ToJSON ContentBlock where
  toJSON (TextBlock txt cc) =
    object
      $ catMaybes
        [ Just ("type" .= ("text" :: Text))
        , Just ("text" .= txt)
        , ("cache_control" .=) <$> cc
        ]
  toJSON (ImageBlock src cc) =
    object
      $ catMaybes
        [ Just ("type" .= ("image" :: Text))
        , Just ("source" .= src)
        , ("cache_control" .=) <$> cc
        ]
  toJSON (ToolUseBlock toolId name input cc) =
    object
      $ catMaybes
        [ Just ("type" .= ("tool_use" :: Text))
        , Just ("id" .= toolId)
        , Just ("name" .= name)
        , Just ("input" .= input)
        , ("cache_control" .=) <$> cc
        ]
  toJSON (ToolResultBlock toolId result isErr cc) =
    object
      $ catMaybes
        [ Just ("type" .= ("tool_result" :: Text))
        , Just ("tool_use_id" .= toolId)
        , Just ("content" .= result)
        , ("is_error" .=) <$> isErr
        , ("cache_control" .=) <$> cc
        ]
  toJSON (ThinkingBlock thinking sig cc) =
    object
      $ catMaybes
        [ Just ("type" .= ("thinking" :: Text))
        , Just ("thinking" .= thinking)
        , Just ("signature" .= sig)
        , ("cache_control" .=) <$> cc
        ]

-- | Smart constructor for text blocks
textBlock :: Text -> ContentBlock
textBlock t = TextBlock t Nothing

-- | Smart constructor for image blocks from base64 data
imageBlock :: Text -> Text -> ContentBlock
imageBlock mediaType b64Data =
  ImageBlock (Base64Source mediaType b64Data) Nothing

-- | Smart constructor for tool use blocks
--
-- Accepts a JSON Object containing the tool parameters:
-- @
-- toolUseBlock toolId "get_weather" (object [("location", String "SF")])
-- @
toolUseBlock :: ToolCallId -> Text -> Object -> ContentBlock
toolUseBlock tid n i = ToolUseBlock tid n (ToolUseInput i) Nothing

-- | Smart constructor for text tool results
--
-- Use this for simple string results:
-- @
-- toolResultText toolId "The weather is 72°F" Nothing
-- @
toolResultText :: ToolCallId -> Text -> Maybe Bool -> ContentBlock
toolResultText tid txt e = ToolResultBlock tid (ToolResultText txt) e Nothing

-- | Smart constructor for rich tool results with content blocks
--
-- Use this for results with images, formatted text, etc.:
-- @
-- toolResultBlocks toolId [textBlock "Result:", imageBlock "image/png" base64Data] Nothing
-- @
toolResultBlocks :: ToolCallId -> [ContentBlock] -> Maybe Bool -> ContentBlock
toolResultBlocks tid blocks e = ToolResultBlock tid (ToolResultBlocks blocks) e Nothing

-- | Smart constructor for thinking blocks
thinkingBlock :: Text -> Text -> ContentBlock
thinkingBlock thinking sig = ThinkingBlock thinking sig Nothing

-- | Add cache control to any content block
withCacheControl :: CacheControl -> ContentBlock -> ContentBlock
withCacheControl cc block = block {blockCacheControl = Just cc}

-- | The "ephemeral" cache control value (currently the only supported type)
ephemeralCacheControl :: CacheControl
ephemeralCacheControl = CacheControl "ephemeral"
