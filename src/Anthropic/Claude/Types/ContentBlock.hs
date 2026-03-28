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

* 'ContentBlock' and its 6 constructors (TextBlock, ImageBlock, DocumentBlock, ToolUseBlock, ToolResultBlock, ThinkingBlock)
* Direct dependencies of ContentBlock (CacheControl, ImageSource, DocumentSource, Citation, CitationsConfig, ToolResultContent, ToolUseInput)
* Helper constructors and combinators
-}
module Anthropic.Claude.Types.ContentBlock
  ( -- * Content Types
    ContentBlock (..)
  , ImageSource (..)
  , DocumentSource (..)
  , Citation (..)
  , CitationsConfig (..)
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
  , documentBlock
  , documentBlockUrl
  , documentBlockText

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

-- | Document source for PDF and other document types
--
-- Documents can be provided as base64-encoded data, plain text, custom
-- content blocks, URLs, or file references.
data DocumentSource
  = DocBase64Source
      { docMediaType :: Text
      -- ^ MIME type (e.g., "application/pdf")
      , docData :: Text
      -- ^ Base64-encoded document data
      }
  | DocTextSource
      { docTextMediaType :: Text
      -- ^ MIME type (e.g., "text/plain")
      , docTextData :: Text
      -- ^ Plain text content
      }
  | DocContentSource
      { docContent :: [Value]
      -- ^ Content blocks (e.g., @[{"type":"text","text":"..."}]@)
      }
  | DocURLSource
      { docUrl :: Text
      -- ^ URL to document (must be accessible)
      }
  | DocFileSource
      { docFileId :: Text
      -- ^ File ID from the Files API
      }
  deriving (Eq, Show, Generic)

instance FromJSON DocumentSource where
  parseJSON = withDiscriminator "type" $ \typeField obj -> case typeField of
    "base64" ->
      DocBase64Source
        <$> obj .: "media_type"
        <*> obj .: "data"
    "text" ->
      DocTextSource
        <$> obj .: "media_type"
        <*> obj .: "data"
    "content" ->
      DocContentSource
        <$> obj .: "content"
    "url" ->
      DocURLSource
        <$> obj .: "url"
    "file" ->
      DocFileSource
        <$> obj .: "file_id"
    other -> fail $ "Unknown DocumentSource type: " <> T.unpack other

instance ToJSON DocumentSource where
  toJSON (DocBase64Source mediaType docData') =
    object
      [ "type" .= ("base64" :: Text)
      , "media_type" .= mediaType
      , "data" .= docData'
      ]
  toJSON (DocTextSource mediaType textData) =
    object
      [ "type" .= ("text" :: Text)
      , "media_type" .= mediaType
      , "data" .= textData
      ]
  toJSON (DocContentSource content) =
    object
      [ "type" .= ("content" :: Text)
      , "content" .= content
      ]
  toJSON (DocURLSource url) =
    object
      [ "type" .= ("url" :: Text)
      , "url" .= url
      ]
  toJSON (DocFileSource fileId) =
    object
      [ "type" .= ("file" :: Text)
      , "file_id" .= fileId
      ]

-- | Citation configuration for document blocks
newtype CitationsConfig = CitationsConfig
  { citationsEnabled :: Bool
  }
  deriving (Eq, Show, Generic)

instance FromJSON CitationsConfig where
  parseJSON = genericParseJSON (prefixOptions "citations")

instance ToJSON CitationsConfig where
  toJSON = genericToJSON (prefixOptions "citations")
  toEncoding = genericToEncoding (prefixOptions "citations")

-- | A citation referencing a source document location
--
-- Citations appear in TextBlock responses when citations are enabled.
-- Three location types correspond to the three document source types.
data Citation
  = -- | Character-level citation for plain text documents
    CharLocation
      { citedText :: Text
      , citationDocumentIndex :: Int
      , citationDocumentTitle :: Maybe Text
      , citationStartCharIndex :: Int
      , citationEndCharIndex :: Int
      }
  | -- | Page-level citation for PDF documents
    PageLocation
      { citedText :: Text
      , citationDocumentIndex :: Int
      , citationDocumentTitle :: Maybe Text
      , citationStartPageNumber :: Int
      , citationEndPageNumber :: Int
      }
  | -- | Block-level citation for custom content documents
    ContentBlockLocation
      { citedText :: Text
      , citationDocumentIndex :: Int
      , citationDocumentTitle :: Maybe Text
      , citationStartBlockIndex :: Int
      , citationEndBlockIndex :: Int
      }
  deriving (Eq, Show, Generic)

instance FromJSON Citation where
  parseJSON = withDiscriminator "type" $ \typeField obj -> case typeField of
    "char_location" ->
      CharLocation
        <$> obj .: "cited_text"
        <*> obj .: "document_index"
        <*> obj .:? "document_title"
        <*> obj .: "start_char_index"
        <*> obj .: "end_char_index"
    "page_location" ->
      PageLocation
        <$> obj .: "cited_text"
        <*> obj .: "document_index"
        <*> obj .:? "document_title"
        <*> obj .: "start_page_number"
        <*> obj .: "end_page_number"
    "content_block_location" ->
      ContentBlockLocation
        <$> obj .: "cited_text"
        <*> obj .: "document_index"
        <*> obj .:? "document_title"
        <*> obj .: "start_block_index"
        <*> obj .: "end_block_index"
    other -> fail $ "Unknown Citation type: " <> T.unpack other

instance ToJSON Citation where
  toJSON (CharLocation cited docIdx docTitle startIdx endIdx) =
    object
      $ catMaybes
        [ Just ("type" .= ("char_location" :: Text))
        , Just ("cited_text" .= cited)
        , Just ("document_index" .= docIdx)
        , ("document_title" .=) <$> docTitle
        , Just ("start_char_index" .= startIdx)
        , Just ("end_char_index" .= endIdx)
        ]
  toJSON (PageLocation cited docIdx docTitle startPage endPage) =
    object
      $ catMaybes
        [ Just ("type" .= ("page_location" :: Text))
        , Just ("cited_text" .= cited)
        , Just ("document_index" .= docIdx)
        , ("document_title" .=) <$> docTitle
        , Just ("start_page_number" .= startPage)
        , Just ("end_page_number" .= endPage)
        ]
  toJSON (ContentBlockLocation cited docIdx docTitle startBlk endBlk) =
    object
      $ catMaybes
        [ Just ("type" .= ("content_block_location" :: Text))
        , Just ("cited_text" .= cited)
        , Just ("document_index" .= docIdx)
        , ("document_title" .=) <$> docTitle
        , Just ("start_block_index" .= startBlk)
        , Just ("end_block_index" .= endBlk)
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
-- * 'DocumentBlock' - PDF or other document (base64, URL, or file reference)
data ContentBlock
  = TextBlock
      { blockText :: Text
      -- ^ Text content
      , blockCitations :: Maybe [Citation]
      -- ^ Citations referencing source documents (response only)
      , blockCacheControl :: Maybe CacheControl
      -- ^ Cache control for prompt caching
      }
  | ImageBlock
      { blockSource :: ImageSource
      -- ^ Image source (base64 or URL)
      , blockCacheControl :: Maybe CacheControl
      -- ^ Cache control for prompt caching
      }
  | DocumentBlock
      { blockDocSource :: DocumentSource
      -- ^ Document source (base64, URL, text, content, or file)
      , blockDocTitle :: Maybe Text
      -- ^ Optional document title (not citable)
      , blockDocContext :: Maybe Text
      -- ^ Optional context about the document (not citable)
      , blockDocCitations :: Maybe CitationsConfig
      -- ^ Enable citations for this document
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
        <*> obj .:? "citations"
        <*> obj .:? "cache_control"
    "image" ->
      ImageBlock
        <$> obj .: "source"
        <*> obj .:? "cache_control"
    "document" ->
      DocumentBlock
        <$> obj .: "source"
        <*> obj .:? "title"
        <*> obj .:? "context"
        <*> obj .:? "citations"
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
  toJSON (TextBlock txt cits cc) =
    object
      $ catMaybes
        [ Just ("type" .= ("text" :: Text))
        , Just ("text" .= txt)
        , ("citations" .=) <$> cits
        , ("cache_control" .=) <$> cc
        ]
  toJSON (ImageBlock src cc) =
    object
      $ catMaybes
        [ Just ("type" .= ("image" :: Text))
        , Just ("source" .= src)
        , ("cache_control" .=) <$> cc
        ]
  toJSON (DocumentBlock src title ctx cits cc) =
    object
      $ catMaybes
        [ Just ("type" .= ("document" :: Text))
        , Just ("source" .= src)
        , ("title" .=) <$> title
        , ("context" .=) <$> ctx
        , ("citations" .=) <$> cits
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
textBlock t = TextBlock t Nothing Nothing

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

-- | Smart constructor for document blocks from base64 data
documentBlock :: Text -> Text -> ContentBlock
documentBlock mediaType b64Data =
  DocumentBlock (DocBase64Source mediaType b64Data) Nothing Nothing Nothing Nothing

-- | Smart constructor for document blocks from a URL
documentBlockUrl :: Text -> ContentBlock
documentBlockUrl url = DocumentBlock (DocURLSource url) Nothing Nothing Nothing Nothing

-- | Smart constructor for document blocks from plain text
documentBlockText :: Text -> ContentBlock
documentBlockText txt =
  DocumentBlock (DocTextSource "text/plain" txt) Nothing Nothing Nothing Nothing

-- | Add cache control to any content block
withCacheControl :: CacheControl -> ContentBlock -> ContentBlock
withCacheControl cc block = block {blockCacheControl = Just cc}

-- | The "ephemeral" cache control value (currently the only supported type)
ephemeralCacheControl :: CacheControl
ephemeralCacheControl = CacheControl "ephemeral"
