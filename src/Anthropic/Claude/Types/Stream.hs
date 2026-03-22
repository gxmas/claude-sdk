{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Anthropic.Claude.Types.Stream
Description : Streaming event types for Claude API
Copyright   : (c) 2026 Geoffrey Noël
License     : MIT
Maintainer  : noel.geoff@gmail.com

Types for Claude API streaming (Server-Sent Events). The API sends
a sequence of typed events during streaming message creation.

Per ADR 0005, streams yield @Either APIError StreamEvent@ to allow
partial recovery from mid-stream errors.
-}
module Anthropic.Claude.Types.Stream
  ( -- * Stream Events
    StreamEvent(..)
  , ContentDelta(..)

    -- * Event Payloads
  , MessageStartPayload(..)
  , ContentBlockStartPayload(..)
  , ContentBlockDeltaPayload(..)
  , MessageDeltaPayload(..)
  , DeltaBody(..)
  ) where

import Anthropic.Claude.Internal.JSON
import Anthropic.Claude.Types.Common
import Anthropic.Claude.Types.Core
import Anthropic.Claude.Types.Response (Usage, MessageResponse)
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Incremental content update within a streaming content block.
data ContentDelta
  = TextDelta Text           -- ^ Incremental text fragment
  | InputJsonDelta Text      -- ^ Incremental JSON fragment for tool input
  deriving (Eq, Show, Generic)

instance FromJSON ContentDelta where
  parseJSON = withDiscriminator "type" $ \typeField obj -> case typeField of
    "text_delta"       -> TextDelta <$> obj .: "text"
    "input_json_delta" -> InputJsonDelta <$> obj .: "partial_json"
    other              -> fail $ "Unknown ContentDelta type: " <> T.unpack other

instance ToJSON ContentDelta where
  toJSON (TextDelta txt) = object
    [ "type" .= ("text_delta" :: Text), "text" .= txt ]
  toJSON (InputJsonDelta json) = object
    [ "type" .= ("input_json_delta" :: Text), "partial_json" .= json ]

-- | Payload for a @message_start@ event.
data MessageStartPayload = MessageStartPayload
  { messageStartMessage :: MessageResponse
  } deriving (Eq, Show, Generic)

instance FromJSON MessageStartPayload where
  parseJSON = withObject "MessageStartPayload" $ \obj ->
    MessageStartPayload <$> obj .: "message"

instance ToJSON MessageStartPayload where
  toJSON (MessageStartPayload msg) = object [ "message" .= msg ]

-- | Payload for a @content_block_start@ event.
data ContentBlockStartPayload = ContentBlockStartPayload
  { contentBlockStartIndex :: Int
  , contentBlockStartBlock :: ContentBlock
  } deriving (Eq, Show, Generic)

instance FromJSON ContentBlockStartPayload where
  parseJSON = withObject "ContentBlockStartPayload" $ \obj ->
    ContentBlockStartPayload <$> obj .: "index" <*> obj .: "content_block"

instance ToJSON ContentBlockStartPayload where
  toJSON (ContentBlockStartPayload idx blk) = object
    [ "index" .= idx, "content_block" .= blk ]

-- | Payload for a @content_block_delta@ event.
data ContentBlockDeltaPayload = ContentBlockDeltaPayload
  { contentBlockDeltaIndex :: Int
  , contentBlockDeltaDelta :: ContentDelta
  } deriving (Eq, Show, Generic)

instance FromJSON ContentBlockDeltaPayload where
  parseJSON = withObject "ContentBlockDeltaPayload" $ \obj ->
    ContentBlockDeltaPayload <$> obj .: "index" <*> obj .: "delta"

instance ToJSON ContentBlockDeltaPayload where
  toJSON (ContentBlockDeltaPayload idx d) = object
    [ "index" .= idx, "delta" .= d ]

-- | Body carried inside a @message_delta@ event.
data DeltaBody = DeltaBody
  { deltaStopReason   :: Maybe StopReason
  , deltaStopSequence :: Maybe Text
  } deriving (Eq, Show, Generic)

instance FromJSON DeltaBody where
  parseJSON = withObject "DeltaBody" $ \obj ->
    DeltaBody <$> obj .:? "stop_reason" <*> obj .:? "stop_sequence"

instance ToJSON DeltaBody where
  toJSON (DeltaBody sr ss) = object $
    [ "stop_reason"   .= sr ] ++
    maybe [] (\s -> [ "stop_sequence" .= s ]) ss

-- | Payload for a @message_delta@ event.
data MessageDeltaPayload = MessageDeltaPayload
  { messageDeltaDelta :: DeltaBody
  , messageDeltaUsage :: Usage
  } deriving (Eq, Show, Generic)

instance FromJSON MessageDeltaPayload where
  parseJSON = withObject "MessageDeltaPayload" $ \obj ->
    MessageDeltaPayload <$> obj .: "delta" <*> obj .: "usage"

instance ToJSON MessageDeltaPayload where
  toJSON (MessageDeltaPayload d u) = object
    [ "delta" .= d, "usage" .= u ]

-- | A single event received during streaming message creation.
--
-- The Claude API sends these events over an SSE connection in order:
--
-- 1. 'MessageStart'        — initial message metadata
-- 2. 'ContentBlockStart'   — beginning of a content block
-- 3. 'ContentBlockDelta'   — incremental content (text or tool-input)
-- 4. 'ContentBlockStop'    — end of a content block
-- 5. 'MessageDelta'        — final metadata (stop reason, usage)
-- 6. 'MessageStop'         — end of message
--
-- Additionally:
--
-- * 'Ping'                 — keep-alive sent periodically
-- * 'StreamError'          — mid-stream error
data StreamEvent
  = MessageStart       MessageStartPayload
  | ContentBlockStart  ContentBlockStartPayload
  | ContentBlockDelta  ContentBlockDeltaPayload
  | ContentBlockStop   { contentBlockStopIndex :: Int }
  | MessageDelta       MessageDeltaPayload
  | MessageStop
  | Ping
  | StreamError        { streamErrorMessage :: Text }
  deriving (Eq, Show, Generic)

instance FromJSON StreamEvent where
  parseJSON = withDiscriminator "type" $ \typeField obj -> case typeField of
    "message_start"       -> MessageStart <$> parseJSON (Object obj)
    "content_block_start" -> ContentBlockStart <$> parseJSON (Object obj)
    "content_block_delta" -> ContentBlockDelta <$> parseJSON (Object obj)
    "content_block_stop"  -> ContentBlockStop <$> obj .: "index"
    "message_delta"       -> MessageDelta <$> parseJSON (Object obj)
    "message_stop"        -> pure MessageStop
    "ping"                -> pure Ping
    "error"               -> StreamError <$> (obj .: "error" >>= (.: "message"))
    other                 -> fail $ "Unknown StreamEvent type: " <> T.unpack other

instance ToJSON StreamEvent where
  toJSON (MessageStart p) = mergeType "message_start" (toJSON p)
  toJSON (ContentBlockStart p) = mergeType "content_block_start" (toJSON p)
  toJSON (ContentBlockDelta p) = mergeType "content_block_delta" (toJSON p)
  toJSON (ContentBlockStop idx) = object
    [ "type" .= ("content_block_stop" :: Text), "index" .= idx ]
  toJSON (MessageDelta p) = mergeType "message_delta" (toJSON p)
  toJSON MessageStop = object [ "type" .= ("message_stop" :: Text) ]
  toJSON Ping = object [ "type" .= ("ping" :: Text) ]
  toJSON (StreamError msg) = object
    [ "type" .= ("error" :: Text)
    , "error" .= object [ "message" .= msg ]
    ]

-- | Helper: merge a @"type"@ key into an existing JSON object.
mergeType :: Text -> Value -> Value
mergeType t (Object obj) = Object (KM.insert "type" (toJSON t) obj)
mergeType t _ = object [ "type" .= t ]
