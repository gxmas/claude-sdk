{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Anthropic.Claude.Types.Common
-- Description : Common types shared across requests and responses
-- Copyright   : (c) 2026 Geoffrey Noël
-- License     : MIT
-- Maintainer  : noel.geoff@gmail.com
--
-- Common types shared across multiple modules, including content blocks,
-- cache control, and rate limit information. This module breaks potential
-- circular dependencies by housing types that would otherwise cause
-- import cycles between Types.Client and Types.Observability/Logging.
module Anthropic.Claude.Types.Common
  ( -- * Content Types
    ContentBlock (..),
    ImageSource (..),
    MessageContent (..),
    CacheControl (..),
    ToolResultContent (..),
    ToolUseInput (..),

    -- * Rate Limiting
    RateLimitInfo (..),

    -- * Helper Constructors
    textBlock,
    imageBlock,
    toolUseBlock,
    toolResultText,
    toolResultBlocks,

    -- * Cache Control Helpers
    withCacheControl,
    ephemeralCacheControl,
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
  { -- | Currently only "ephemeral" is supported
    cacheType :: Text
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
      { -- | MIME type (e.g., "image/jpeg", "image/png")
        base64MediaType :: Text,
        -- | Base64-encoded image data
        base64Data :: Text
      }
  | URLSource
      { -- | URL to image (must be accessible)
        imageUrl :: Text
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
      [ "type" .= ("base64" :: Text),
        "media_type" .= mediaType,
        "data" .= base64
      ]
  toJSON (URLSource url) =
    object
      [ "type" .= ("url" :: Text),
        "url" .= url
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
  { -- | JSON object containing tool parameters
    unToolUseInput :: Object
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
data ContentBlock
  = TextBlock
      { -- | Text content
        blockText :: Text,
        -- | Cache control for prompt caching
        blockCacheControl :: Maybe CacheControl
      }
  | ImageBlock
      { -- | Image source (base64 or URL)
        blockSource :: ImageSource,
        -- | Cache control for prompt caching
        blockCacheControl :: Maybe CacheControl
      }
  | ToolUseBlock
      { -- | Unique identifier for this tool use
        blockToolUseId :: ToolCallId,
        -- | Name of the tool to call
        blockToolName :: Text,
        -- | JSON object containing tool parameters
        blockToolInput :: ToolUseInput,
        -- | Cache control for prompt caching
        blockCacheControl :: Maybe CacheControl
      }
  | ToolResultBlock
      { -- | ID matching the ToolUseBlock
        blockToolResultId :: ToolCallId,
        -- | Tool execution result
        blockToolResult :: ToolResultContent,
        -- | Whether the tool call resulted in an error
        blockIsError :: Maybe Bool,
        -- | Cache control for prompt caching
        blockCacheControl :: Maybe CacheControl
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
    other -> fail $ "Unknown ContentBlock type: " <> T.unpack other

instance ToJSON ContentBlock where
  toJSON (TextBlock txt cc) =
    object $
      catMaybes
        [ Just ("type" .= ("text" :: Text)),
          Just ("text" .= txt),
          ("cache_control" .=) <$> cc
        ]
  toJSON (ImageBlock src cc) =
    object $
      catMaybes
        [ Just ("type" .= ("image" :: Text)),
          Just ("source" .= src),
          ("cache_control" .=) <$> cc
        ]
  toJSON (ToolUseBlock toolId name input cc) =
    object $
      catMaybes
        [ Just ("type" .= ("tool_use" :: Text)),
          Just ("id" .= toolId),
          Just ("name" .= name),
          Just ("input" .= input),
          ("cache_control" .=) <$> cc
        ]
  toJSON (ToolResultBlock toolId result isErr cc) =
    object $
      catMaybes
        [ Just ("type" .= ("tool_result" :: Text)),
          Just ("tool_use_id" .= toolId),
          Just ("content" .= result),
          ("is_error" .=) <$> isErr,
          ("cache_control" .=) <$> cc
        ]

-- | Message content (either text or list of content blocks)
--
-- For simple cases, messages can contain just a text string.
-- For complex cases (images, tool use), use a list of ContentBlocks.
data MessageContent
  = TextContent Text
  | BlocksContent [ContentBlock]
  deriving (Eq, Show, Generic)

instance FromJSON MessageContent where
  parseJSON v =
    (TextContent <$> parseJSON v)
      <|> (BlocksContent <$> parseJSON v)

instance ToJSON MessageContent where
  toJSON (TextContent t) = toJSON t
  toJSON (BlocksContent blocks) = toJSON blocks

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

-- | Add cache control to any content block
withCacheControl :: CacheControl -> ContentBlock -> ContentBlock
withCacheControl cc block = block {blockCacheControl = Just cc}

-- | The "ephemeral" cache control value (currently the only supported type)
ephemeralCacheControl :: CacheControl
ephemeralCacheControl = CacheControl "ephemeral"

-- | Rate limit information from API response headers
--
-- Per ADR 0004, rate limit metadata is carried in APIResponse wrapper.
-- All fields are Maybe because not all endpoints return all headers.
data RateLimitInfo = RateLimitInfo
  { -- | Requests allowed in window
    rateLimitRequests :: Maybe Int,
    -- | Tokens allowed in window
    rateLimitTokens :: Maybe Int,
    -- | Requests remaining
    rateLimitRemaining :: Maybe Int,
    -- | Tokens remaining
    rateLimitTokensRemaining :: Maybe Int,
    -- | Seconds until request limit resets
    rateLimitResetRequests :: Maybe Int,
    -- | Seconds until token limit resets
    rateLimitResetTokens :: Maybe Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON RateLimitInfo where
  parseJSON = genericParseJSON (prefixOptions "rateLimit")

instance ToJSON RateLimitInfo where
  toJSON = genericToJSON (prefixOptions "rateLimit")
  toEncoding = genericToEncoding (prefixOptions "rateLimit")
