{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Anthropic.Claude.Types.Core
Description : Core types for Claude SDK
Copyright   : (c) 2026 Geoffrey Noël
License     : MIT
Maintainer  : noel.geoff@gmail.com

Core types used throughout the SDK including newtypes for API identifiers,
enumerations for roles and stop reasons, and model ID constants.
-}
module Anthropic.Claude.Types.Core
  ( -- * Identifier Types
    ApiKey (..)
  , ModelId (..)
  , RequestId (..)
  , MessageId (..)
  , BatchId (..)
  , ToolCallId (..)

    -- * Enumerations
  , Role (..)
  , StopReason (..)

    -- * Model Constants
  , claude4Opus
  , claude4Sonnet
  , claude35Haiku
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | API key for authenticating with the Claude API
--
-- Obtain from: https://console.anthropic.com/settings/keys
newtype ApiKey = ApiKey {unApiKey :: Text}
  deriving (Eq, Show, Generic, IsString, FromJSON, ToJSON)

-- | Claude model identifier
--
-- Examples: @claude-opus-4-6@, @claude-sonnet-4-6@, @claude-haiku-4-5@
newtype ModelId = ModelId {unModelId :: Text}
  deriving (Eq, Show, Generic, IsString, FromJSON, ToJSON)

-- | Unique identifier for an API request
--
-- Returned in the @x-request-id@ header and can be used for debugging
newtype RequestId = RequestId {unRequestId :: Text}
  deriving (Eq, Show, Generic, IsString, FromJSON, ToJSON)

-- | Unique identifier for a message
newtype MessageId = MessageId {unMessageId :: Text}
  deriving (Eq, Show, Generic, IsString, FromJSON, ToJSON)

-- | Unique identifier for a batch operation
newtype BatchId = BatchId {unBatchId :: Text}
  deriving (Eq, Show, Generic, IsString, FromJSON, ToJSON)

-- | Unique identifier for a tool call
newtype ToolCallId = ToolCallId {unToolCallId :: Text}
  deriving (Eq, Show, Generic, IsString, FromJSON, ToJSON)

-- | Role in a conversation
data Role
  = -- | Human message
    User
  | -- | Claude's response
    Assistant
  deriving (Eq, Show, Generic, Enum, Bounded)

instance FromJSON Role where
  parseJSON = withText "Role" $ \case
    "user" -> pure User
    "assistant" -> pure Assistant
    other -> fail $ "Unknown role: " <> T.unpack other

instance ToJSON Role where
  toJSON User = "user"
  toJSON Assistant = "assistant"

-- | Reason why Claude stopped generating
data StopReason
  = -- | Natural completion point
    EndTurn
  | -- | Maximum token limit reached
    MaxTokens
  | -- | Custom stop sequence encountered
    StopSequence
  | -- | Model wants to use a tool
    ToolUse
  deriving (Eq, Show, Generic, Enum, Bounded)

instance FromJSON StopReason where
  parseJSON = withText "StopReason" $ \case
    "end_turn" -> pure EndTurn
    "max_tokens" -> pure MaxTokens
    "stop_sequence" -> pure StopSequence
    "tool_use" -> pure ToolUse
    other -> fail $ "Unknown stop reason: " <> T.unpack other

instance ToJSON StopReason where
  toJSON EndTurn = "end_turn"
  toJSON MaxTokens = "max_tokens"
  toJSON StopSequence = "stop_sequence"
  toJSON ToolUse = "tool_use"

-- | Claude Opus 4.6 - Most capable model
claude4Opus :: ModelId
claude4Opus = "claude-opus-4-6"

-- | Claude Sonnet 4.6 - Balanced performance
claude4Sonnet :: ModelId
claude4Sonnet = "claude-sonnet-4-6"

-- | Claude 3.5 Haiku - Fastest model
claude35Haiku :: ModelId
claude35Haiku = "claude-haiku-4-5-20251001"
