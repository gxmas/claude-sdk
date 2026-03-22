{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Anthropic.Claude.Types.Response
Description : Response types for Claude API
Copyright   : (c) 2026 Anthropic
License     : MIT
Maintainer  : gnoel@anthropic.com

Response types for the Messages API including MessageResponse,
Usage statistics, and the APIResponse wrapper that carries
rate limit metadata per ADR 0004.
-}
module Anthropic.Claude.Types.Response
  ( -- * Response Types
    MessageResponse(..)
  , Usage(..)
  , APIResponse(..)

    -- * Helper Functions
  , extractText
  , unwrapOrThrow
  ) where

import Anthropic.Claude.Internal.JSON
import Anthropic.Claude.Types.Client (RateLimitInfo)
import Anthropic.Claude.Types.Common
import Anthropic.Claude.Types.Core
import Control.Exception (Exception, throwIO)
import Data.Aeson.KeyMap (toList)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Token usage statistics
data Usage = Usage
  { usageInputTokens :: Int
  , usageOutputTokens :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON Usage where
  parseJSON = genericParseJSON (prefixOptions "usage")

instance ToJSON Usage where
  toJSON = genericToJSON (prefixOptions "usage")
  toEncoding = genericToEncoding (prefixOptions "usage")

-- | Response from the Messages API
data MessageResponse = MessageResponse
  { responseId :: MessageId
  , responseType :: Text                -- ^ Always "message"
  , responseRole :: Role                -- ^ Always Assistant
  , responseContent :: [ContentBlock]
  , responseModel :: ModelId
  , responseStopReason :: Maybe StopReason
  , responseStopSequence :: Maybe Text
  , responseUsage :: Usage
  } deriving (Eq, Show, Generic)

instance FromJSON MessageResponse where
  parseJSON = genericParseJSON (prefixOptions "response")

instance ToJSON MessageResponse where
  toJSON = genericToJSON (prefixOptions "response")
  toEncoding = genericToEncoding (prefixOptions "response")

-- | API response wrapper carrying rate limit metadata
--
-- Per ADR 0004, all API functions return this wrapper which includes:
-- - The response body (type parameter @a@)
-- - Rate limit information from response headers
-- - Request ID for debugging
data APIResponse a = APIResponse
  { apiResponseBody :: a
  , apiResponseRateLimitInfo :: Maybe RateLimitInfo
  , apiResponseRequestId :: Maybe RequestId
  } deriving (Eq, Show, Generic)

instance FromJSON a => FromJSON (APIResponse a) where
  parseJSON = withObject "APIResponse" $ \obj ->
    APIResponse
      <$> parseJSON (Object obj)
      <*> obj .:? "rate_limit_info"
      <*> obj .:? "request_id"

instance ToJSON a => ToJSON (APIResponse a) where
  toJSON (APIResponse body rateLimitInfo reqId) = object $
    case toJSON body of
      Object obj -> toList obj ++
        [ "rate_limit_info" .= rateLimitInfo
        , "request_id" .= reqId
        ]
      _ -> []

-- | Extract text content from a message response
--
-- Concatenates all TextBlock content with newlines.
-- Ignores other block types (images, tool use, etc.).
--
-- Example:
-- @
-- let text = extractText response
-- @
extractText :: MessageResponse -> Text
extractText response = T.intercalate "\n" texts
  where
    texts = [t | TextBlock t <- responseContent response]

-- | Unwrap APIResponse or throw exception
--
-- Useful when you want to ignore rate limit metadata.
--
-- Example:
-- @
-- body <- unwrapOrThrow apiResponse
-- @
unwrapOrThrow :: Exception e => Either e (APIResponse a) -> IO a
unwrapOrThrow (Left err) = throwIO err
unwrapOrThrow (Right resp) = pure $ apiResponseBody resp
