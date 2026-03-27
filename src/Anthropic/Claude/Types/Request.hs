{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Anthropic.Claude.Types.Request
Description : Request types for Claude API
Copyright   : (c) 2026 Geoffrey Noël
License     : MIT
Maintainer  : noel.geoff@gmail.com

Request types for the Messages API including CreateMessageRequest,
Tool definitions, and helper constructors for building requests.
-}
module Anthropic.Claude.Types.Request
  ( -- * Request Types
    CreateMessageRequest(..)
  , Message(..)
  , MessageContent(..)
  , Tool(..)
  , ToolChoice(..)
  , SystemContent(..)
  , SystemBlock(..)

    -- * Helper Constructors
  , userMsg
  , assistantMsg
  , assistantMsgBlocks
  , mkRequest
  , systemBlock
  , cachedSystemBlock
  ) where

import Anthropic.Claude.Internal.JSON
import Anthropic.Claude.Types.ContentBlock
import Anthropic.Claude.Types.Core
import Anthropic.Claude.Types.Schema (JsonSchema)
import Control.Applicative ((<|>))
import Data.Aeson.Types (Parser)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

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

-- | A message in the conversation
data Message = Message
  { messageRole :: Role
  , messageContent :: MessageContent
  } deriving (Eq, Show, Generic)

instance FromJSON Message where
  parseJSON = genericParseJSON (prefixOptions "message")

instance ToJSON Message where
  toJSON = genericToJSON (prefixOptions "message")
  toEncoding = genericToEncoding (prefixOptions "message")

-- | Tool definition for tool use
--
-- Per the official Anthropic schema, tools always have @type: "custom"@,
-- an optional description, a required input_schema, and optional cache_control.
data Tool = Tool
  { toolName         :: Text
  , toolDescription  :: Maybe Text
  , toolInputSchema  :: JsonSchema
  , toolCacheControl :: Maybe CacheControl
  } deriving (Eq, Show)

instance FromJSON Tool where
  parseJSON = withObject "Tool" $ \o -> do
    mType <- o .:? "type" :: Parser (Maybe Text)
    case mType of
      Just t | t /= "custom" -> fail $ "Expected tool type \"custom\", got: " <> T.unpack t
      _ -> pure ()
    Tool
      <$> o .:  "name"
      <*> o .:? "description"
      <*> o .:  "input_schema"
      <*> o .:? "cache_control"

instance ToJSON Tool where
  toJSON (Tool name desc schema cc) = object $ catMaybes
    [ Just ("type"         .= ("custom" :: Text))
    , Just ("name"         .= name)
    , ("description"   .=) <$> desc
    , Just ("input_schema" .= schema)
    , ("cache_control" .=) <$> cc
    ]

-- | Tool choice strategy
data ToolChoice
  = AutoChoice         -- ^ Let Claude decide whether to use tools
  | AnyChoice          -- ^ Claude must use a tool
  | ToolChoice Text    -- ^ Claude must use the specified tool
  deriving (Eq, Show, Generic)

instance FromJSON ToolChoice where
  parseJSON = withDiscriminator "type" $ \typeField obj -> case typeField of
    "auto" -> pure AutoChoice
    "any" -> pure AnyChoice
    "tool" -> ToolChoice <$> obj .: "name"
    other -> fail $ "Unknown ToolChoice type: " <> T.unpack other

instance ToJSON ToolChoice where
  toJSON AutoChoice = object ["type" .= ("auto" :: Text)]
  toJSON AnyChoice = object ["type" .= ("any" :: Text)]
  toJSON (ToolChoice name) = object
    [ "type" .= ("tool" :: Text)
    , "name" .= name
    ]

-- | A system prompt block with optional cache control
data SystemBlock = SystemBlock
  { systemBlockText :: Text
  , systemBlockCacheControl :: Maybe CacheControl
  } deriving (Eq, Show, Generic)

instance FromJSON SystemBlock where
  parseJSON = withObject "SystemBlock" $ \o ->
    SystemBlock
      <$> o .: "text"
      <*> o .:? "cache_control"

instance ToJSON SystemBlock where
  toJSON (SystemBlock txt cc) = object $ catMaybes
    [ Just ("type" .= ("text" :: Text))
    , Just ("text" .= txt)
    , ("cache_control" .=) <$> cc
    ]

-- | System prompt content (either a plain string or a list of blocks)
--
-- Use 'SystemText' for simple system prompts, or 'SystemBlocks' when
-- you need cache control on individual blocks.
data SystemContent
  = SystemText Text            -- ^ @"system": "string"@
  | SystemBlocks [SystemBlock] -- ^ @"system": [{"type":"text","text":"...","cache_control":...}]@
  deriving (Eq, Show, Generic)

instance FromJSON SystemContent where
  parseJSON v = (SystemText <$> parseJSON v)
            <|> (SystemBlocks <$> parseJSON v)

instance ToJSON SystemContent where
  toJSON (SystemText t) = toJSON t
  toJSON (SystemBlocks blocks) = toJSON blocks

-- | Request to create a message via the Messages API.
--
-- Contains all parameters for a message creation request, including model,
-- conversation history, generation limits, and optional features like tools,
-- system prompts, and sampling parameters.
--
-- Use 'mkRequest' for a simple request, or construct directly for full control.
data CreateMessageRequest = CreateMessageRequest
  { requestModel :: ModelId            -- ^ Model identifier (e.g., @claude4Sonnet@)
  , requestMessages :: [Message]       -- ^ Conversation history (must be non-empty)
  , requestMaxTokens :: Int            -- ^ Maximum tokens to generate (must be positive)
  , requestMetadata :: Maybe Value     -- ^ User-defined metadata (opaque to the API)
  , requestStopSequences :: Maybe [Text] -- ^ Custom stop sequences
  , requestStream :: Maybe Bool        -- ^ Enable server-sent events streaming
  , requestSystem :: Maybe SystemContent -- ^ System prompt (instructions for Claude)
  , requestTemperature :: Maybe Double -- ^ Sampling temperature (0.0-1.0, default 1.0)
  , requestToolChoice :: Maybe ToolChoice -- ^ Tool selection strategy
  , requestTools :: Maybe [Tool]       -- ^ Available tools for Claude to use
  , requestTopK :: Maybe Int           -- ^ Top-k sampling parameter
  , requestTopP :: Maybe Double        -- ^ Nucleus sampling parameter (0.0-1.0)
  } deriving (Eq, Show, Generic)

instance FromJSON CreateMessageRequest where
  parseJSON = withObject "CreateMessageRequest" $ \obj -> do
    model <- obj .: "model"
    messages <- obj .: "messages"
    maxTokens <- obj .: "max_tokens"
    metadata <- obj .:? "metadata"
    stopSeqs <- obj .:? "stop_sequences"
    stream <- obj .:? "stream"
    system <- obj .:? "system"
    temperature <- obj .:? "temperature"
    toolChoice <- obj .:? "tool_choice"
    tools <- obj .:? "tools"
    topK <- obj .:? "top_k"
    topP <- obj .:? "top_p"

    -- Validation
    if null messages
      then fail "messages must not be empty"
      else if maxTokens <= 0
        then fail "max_tokens must be positive"
        else pure $ CreateMessageRequest
          model messages maxTokens metadata stopSeqs stream system
          temperature toolChoice tools topK topP

instance ToJSON CreateMessageRequest where
  toJSON (CreateMessageRequest model messages maxTokens metadata stopSeqs stream system temp toolChoice tools topK topP) =
    object $
      [ "model" .= model
      , "messages" .= messages
      , "max_tokens" .= maxTokens
      ] ++ catMaybes
      [ ("metadata" .=) <$> metadata
      , ("stop_sequences" .=) <$> stopSeqs
      , ("stream" .=) <$> stream
      , ("system" .=) <$> system
      , ("temperature" .=) <$> temp
      , ("tool_choice" .=) <$> toolChoice
      , ("tools" .=) <$> tools
      , ("top_k" .=) <$> topK
      , ("top_p" .=) <$> topP
      ]

-- | Helper: Create a user message with text content
userMsg :: Text -> Message
userMsg text = Message User (TextContent text)

-- | Helper: Create an assistant message with text content
assistantMsg :: Text -> Message
assistantMsg text = Message Assistant (TextContent text)

-- | Helper: Create an assistant message with content blocks
assistantMsgBlocks :: [ContentBlock] -> Message
assistantMsgBlocks blocks = Message Assistant (BlocksContent blocks)

-- | Helper: Create a minimal request
--
-- Example:
-- @
-- let req = mkRequest claude4Sonnet [userMsg "Hello"] 1024
-- @
mkRequest :: ModelId -> [Message] -> Int -> CreateMessageRequest
mkRequest model messages maxTokens = CreateMessageRequest
  { requestModel = model
  , requestMessages = messages
  , requestMaxTokens = maxTokens
  , requestMetadata = Nothing
  , requestStopSequences = Nothing
  , requestStream = Nothing
  , requestSystem = Nothing
  , requestTemperature = Nothing
  , requestToolChoice = Nothing
  , requestTools = Nothing
  , requestTopK = Nothing
  , requestTopP = Nothing
  }

-- | Smart constructor for a system block without cache control
systemBlock :: Text -> SystemBlock
systemBlock txt = SystemBlock txt Nothing

-- | Smart constructor for a cached system block (ephemeral cache control)
cachedSystemBlock :: Text -> SystemBlock
cachedSystemBlock txt = SystemBlock txt (Just ephemeralCacheControl)
