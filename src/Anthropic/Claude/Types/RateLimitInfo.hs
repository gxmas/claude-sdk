{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Anthropic.Claude.Types.RateLimitInfo
Description : Rate limit information from API response headers
Copyright   : (c) 2026 Geoffrey Noël
License     : MIT
Maintainer  : noel.geoff@gmail.com

Rate limit metadata extracted from API response headers.

Per ADR 0004, rate limit information is carried in the APIResponse wrapper
and populated from HTTP response headers. All fields are Maybe because not
all endpoints return all headers.

This module exists independently to avoid circular dependencies between
Types.Client, Types.Observability, and Types.Logging.
-}
module Anthropic.Claude.Types.RateLimitInfo
  ( RateLimitInfo (..)
  ) where

import Anthropic.Claude.Internal.JSON
import GHC.Generics (Generic)

-- | Rate limit information from API response headers
--
-- Per ADR 0004, rate limit metadata is carried in APIResponse wrapper.
-- All fields are Maybe because not all endpoints return all headers.
data RateLimitInfo = RateLimitInfo
  { rateLimitRequests :: Maybe Int
  -- ^ Requests allowed in window
  , rateLimitTokens :: Maybe Int
  -- ^ Tokens allowed in window
  , rateLimitRemaining :: Maybe Int
  -- ^ Requests remaining
  , rateLimitTokensRemaining :: Maybe Int
  -- ^ Tokens remaining
  , rateLimitResetRequests :: Maybe Int
  -- ^ Seconds until request limit resets
  , rateLimitResetTokens :: Maybe Int
  -- ^ Seconds until token limit resets
  }
  deriving (Eq, Show, Generic)

instance FromJSON RateLimitInfo where
  parseJSON = genericParseJSON (prefixOptions "rateLimit")

instance ToJSON RateLimitInfo where
  toJSON = genericToJSON (prefixOptions "rateLimit")
  toEncoding = genericToEncoding (prefixOptions "rateLimit")
