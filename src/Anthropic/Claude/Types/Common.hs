{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Anthropic.Claude.Types.Common
Description : Common types shared across requests and responses
Copyright   : (c) 2026 Geoffrey Noël
License     : MIT
Maintainer  : noel.geoff@gmail.com

Common types shared across multiple modules, including content blocks,
cache control, and rate limit information. This module breaks potential
circular dependencies by housing types that would otherwise cause
import cycles between Types.Client and Types.Observability/Logging.
-}
module Anthropic.Claude.Types.Common
  ( -- * Content Types
    ContentBlock(..)
  , ImageSource(..)
  , MessageContent(..)
  , CacheControl(..)

    -- * Rate Limiting
  , RateLimitInfo(..)

    -- * Helper Constructors
  , textBlock
  , imageBlock
  , toolUseBlock
  , toolResultBlock
  ) where

import Anthropic.Claude.Internal.JSON
import Anthropic.Claude.Types.Core
import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Cache control configuration for prompt caching
--
-- Allows marking content blocks as cacheable to reduce latency and cost
-- on repeated requests. See: https://docs.anthropic.com/en/docs/prompt-caching
data CacheControl = CacheControl
  { cacheType :: Text -- ^ Currently only "ephemeral" is supported
  } deriving (Eq, Show, Generic)

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
      { base64MediaType :: Text   -- ^ MIME type (e.g., "image/jpeg", "image/png")
      , base64Data :: Text        -- ^ Base64-encoded image data
      }
  | URLSource
      { imageUrl :: Text          -- ^ URL to image (must be accessible)
      }
  deriving (Eq, Show, Generic)

instance FromJSON ImageSource where
  parseJSON = withDiscriminator "type" $ \typeField obj -> case typeField of
    "base64" -> Base64Source
      <$> obj .: "media_type"
      <*> obj .: "data"
    "url" -> URLSource
      <$> obj .: "url"
    other -> fail $ "Unknown ImageSource type: " <> T.unpack other

instance ToJSON ImageSource where
  toJSON (Base64Source mediaType base64) = object
    [ "type" .= ("base64" :: Text)
    , "media_type" .= mediaType
    , "data" .= base64
    ]
  toJSON (URLSource url) = object
    [ "type" .= ("url" :: Text)
    , "url" .= url
    ]

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
      { blockText :: Text                    -- ^ Text content
      }
  | ImageBlock
      { blockSource :: ImageSource           -- ^ Image source (base64 or URL)
      }
  | ToolUseBlock
      { blockToolUseId :: ToolCallId         -- ^ Unique identifier for this tool use
      , blockToolName :: Text                -- ^ Name of the tool to call
      , blockToolInput :: Value              -- ^ JSON input for the tool
      }
  | ToolResultBlock
      { blockToolResultId :: ToolCallId      -- ^ ID matching the ToolUseBlock
      , blockToolResult :: Value             -- ^ Tool execution result (JSON)
      , blockIsError :: Maybe Bool           -- ^ Whether the tool call resulted in an error
      }
  deriving (Eq, Show, Generic)

instance FromJSON ContentBlock where
  parseJSON = withDiscriminator "type" $ \typeField obj -> case typeField of
    "text" -> TextBlock
      <$> obj .: "text"
    "image" -> ImageBlock
      <$> obj .: "source"
    "tool_use" -> ToolUseBlock
      <$> obj .: "id"
      <*> obj .: "name"
      <*> obj .: "input"
    "tool_result" -> ToolResultBlock
      <$> obj .: "tool_use_id"
      <*> obj .: "content"
      <*> obj .:? "is_error"
    other -> fail $ "Unknown ContentBlock type: " <> T.unpack other

instance ToJSON ContentBlock where
  toJSON (TextBlock txt) = object
    [ "type" .= ("text" :: Text)
    , "text" .= txt
    ]
  toJSON (ImageBlock src) = object
    [ "type" .= ("image" :: Text)
    , "source" .= src
    ]
  toJSON (ToolUseBlock toolId name input) = object
    [ "type" .= ("tool_use" :: Text)
    , "id" .= toolId
    , "name" .= name
    , "input" .= input
    ]
  toJSON (ToolResultBlock toolId result isErr) = object $
    [ "type" .= ("tool_result" :: Text)
    , "tool_use_id" .= toolId
    , "content" .= result
    ] ++ maybe [] (\e -> ["is_error" .= e]) isErr

-- | Message content (either text or list of content blocks)
--
-- For simple cases, messages can contain just a text string.
-- For complex cases (images, tool use), use a list of ContentBlocks.
data MessageContent
  = TextContent Text
  | BlocksContent [ContentBlock]
  deriving (Eq, Show, Generic)

instance FromJSON MessageContent where
  parseJSON v = (TextContent <$> parseJSON v)
            <|> (BlocksContent <$> parseJSON v)

instance ToJSON MessageContent where
  toJSON (TextContent t) = toJSON t
  toJSON (BlocksContent blocks) = toJSON blocks

-- | Smart constructor for text blocks
textBlock :: Text -> ContentBlock
textBlock = TextBlock

-- | Smart constructor for image blocks from base64 data
imageBlock :: Text -> Text -> ContentBlock
imageBlock mediaType b64Data =
  ImageBlock (Base64Source mediaType b64Data)

-- | Smart constructor for tool use blocks
toolUseBlock :: ToolCallId -> Text -> Value -> ContentBlock
toolUseBlock = ToolUseBlock

-- | Smart constructor for tool result blocks
toolResultBlock :: ToolCallId -> Value -> Maybe Bool -> ContentBlock
toolResultBlock = ToolResultBlock

-- | Rate limit information from API response headers
--
-- Per ADR 0004, rate limit metadata is carried in APIResponse wrapper.
-- All fields are Maybe because not all endpoints return all headers.
data RateLimitInfo = RateLimitInfo
  { rateLimitRequests :: Maybe Int       -- ^ Requests allowed in window
  , rateLimitTokens :: Maybe Int         -- ^ Tokens allowed in window
  , rateLimitRemaining :: Maybe Int      -- ^ Requests remaining
  , rateLimitTokensRemaining :: Maybe Int -- ^ Tokens remaining
  , rateLimitResetRequests :: Maybe Int  -- ^ Seconds until request limit resets
  , rateLimitResetTokens :: Maybe Int    -- ^ Seconds until token limit resets
  } deriving (Eq, Show, Generic)

instance FromJSON RateLimitInfo where
  parseJSON = genericParseJSON (prefixOptions "rateLimit")

instance ToJSON RateLimitInfo where
  toJSON = genericToJSON (prefixOptions "rateLimit")
  toEncoding = genericToEncoding (prefixOptions "rateLimit")
