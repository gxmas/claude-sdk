{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Anthropic.Claude.Types.Request
Description : Request types for Claude API
Copyright   : (c) 2026 Anthropic
License     : MIT
Maintainer  : gnoel@anthropic.com

Request types for the Messages API including CreateMessageRequest,
Tool definitions, and helper constructors for building requests.
-}
module Anthropic.Claude.Types.Request
  ( -- * Request Types
    CreateMessageRequest(..)
  , Message(..)
  , Tool(..)
  , ToolChoice(..)

    -- * Helper Constructors
  , userMsg
  , assistantMsg
  , assistantMsgBlocks
  , mkRequest
  ) where

import Anthropic.Claude.Internal.JSON
import Anthropic.Claude.Types.Common
import Anthropic.Claude.Types.Core
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

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
data Tool = Tool
  { toolName :: Text
  , toolDescription :: Text
  , toolInputSchema :: Value
  } deriving (Eq, Show, Generic)

instance FromJSON Tool where
  parseJSON = genericParseJSON (prefixOptions "tool")

instance ToJSON Tool where
  toJSON = genericToJSON (prefixOptions "tool")
  toEncoding = genericToEncoding (prefixOptions "tool")

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

-- | Request to create a message
data CreateMessageRequest = CreateMessageRequest
  { requestModel :: ModelId
  , requestMessages :: [Message]
  , requestMaxTokens :: Int
  , requestMetadata :: Maybe Value
  , requestStopSequences :: Maybe [Text]
  , requestStream :: Maybe Bool
  , requestSystem :: Maybe Text
  , requestTemperature :: Maybe Double
  , requestToolChoice :: Maybe ToolChoice
  , requestTools :: Maybe [Tool]
  , requestTopK :: Maybe Int
  , requestTopP :: Maybe Double
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
