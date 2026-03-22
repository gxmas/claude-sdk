{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Anthropic.Claude.Types.Client
Description : Client configuration types
Copyright   : (c) 2026 Anthropic
License     : MIT
Maintainer  : gnoel@anthropic.com

Client configuration types including retry policies and rate limit information.

Note: The ClientEnv constructor is hidden in this module and exposed only
through Internal modules. Public users create ClientEnv via mkClientEnv.
-}
module Anthropic.Claude.Types.Client
  ( -- * Retry Configuration
    RetryPolicy(..)
  , BackoffStrategy(..)
  , defaultRetryPolicy
  , noRetries
  , aggressiveRetryPolicy

    -- * Rate Limiting
  , RateLimitInfo(..)

    -- * Client Environment (opaque)
  , ClientEnv
  ) where

import Anthropic.Claude.Internal.JSON
import Anthropic.Claude.Types.Core (ApiKey)
import Data.Time.Clock (NominalDiffTime)
import GHC.Generics (Generic)

-- | Backoff strategy for retries
data BackoffStrategy
  = ExponentialBackoff
      { exponentialBase :: NominalDiffTime  -- ^ Base delay (e.g., 1 second)
      , exponentialMax :: NominalDiffTime   -- ^ Maximum delay cap
      }
  | ConstantBackoff
      { constantDelay :: NominalDiffTime    -- ^ Fixed delay between retries
      }
  | NoBackoff                               -- ^ No delay between retries
  deriving (Eq, Show, Generic)

instance FromJSON BackoffStrategy where
  parseJSON = withDiscriminator "type" $ \typeField obj -> case typeField of
    "exponential" -> ExponentialBackoff
      <$> (fromRational <$> obj .: "base")
      <*> (fromRational <$> obj .: "max")
    "constant" -> ConstantBackoff
      <$> (fromRational <$> obj .: "delay")
    "none" -> pure NoBackoff
    other -> fail $ "Unknown BackoffStrategy: " <> show other

instance ToJSON BackoffStrategy where
  toJSON (ExponentialBackoff base maxDelay) = object
    [ "type" .= ("exponential" :: String)
    , "base" .= (toRational base :: Rational)
    , "max" .= (toRational maxDelay :: Rational)
    ]
  toJSON (ConstantBackoff delay) = object
    [ "type" .= ("constant" :: String)
    , "delay" .= (toRational delay :: Rational)
    ]
  toJSON NoBackoff = object
    [ "type" .= ("none" :: String)
    ]

-- | Retry policy configuration
--
-- Per ADR 0003, retry policy is configured in ClientEnv but can be
-- overridden per-request using withRetryPolicy combinator.
data RetryPolicy = RetryPolicy
  { retryMaxAttempts :: Int              -- ^ Maximum number of retry attempts (0 = no retries)
  , retryStrategy :: BackoffStrategy     -- ^ Backoff strategy between retries
  } deriving (Eq, Show, Generic)

instance FromJSON RetryPolicy where
  parseJSON = genericParseJSON aesonOptions

instance ToJSON RetryPolicy where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

-- | Default retry policy: exponential backoff with 3 retries
--
-- - Max 3 retries
-- - Exponential backoff: 1s base, 60s max
-- - Formula: min(60s, 1s * 2^attempt)
defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy = RetryPolicy
  { retryMaxAttempts = 3
  , retryStrategy = ExponentialBackoff
      { exponentialBase = 1.0    -- 1 second
      , exponentialMax = 60.0    -- 60 seconds cap
      }
  }

-- | No retries policy
--
-- Useful for operations that should fail fast
noRetries :: RetryPolicy
noRetries = RetryPolicy
  { retryMaxAttempts = 0
  , retryStrategy = NoBackoff
  }

-- | Aggressive retry policy for resilient operations
--
-- - Max 5 retries
-- - Exponential backoff: 0.5s base, 120s max
aggressiveRetryPolicy :: RetryPolicy
aggressiveRetryPolicy = RetryPolicy
  { retryMaxAttempts = 5
  , retryStrategy = ExponentialBackoff
      { exponentialBase = 0.5    -- 500ms
      , exponentialMax = 120.0   -- 2 minutes cap
      }
  }

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
  parseJSON = genericParseJSON aesonOptions

instance ToJSON RateLimitInfo where
  toJSON = genericToJSON aesonOptions
  toEncoding = genericToEncoding aesonOptions

-- | Client environment (opaque type)
--
-- Constructor is hidden - use mkClientEnv from Anthropic.Claude.Client
-- to create instances. This prevents misconfiguration.
--
-- Contains:
-- - API key for authentication
-- - HTTP manager for connection pooling
-- - Default retry policy
-- - Base URL (for testing/custom deployments)
data ClientEnv = ClientEnv
  { clientApiKey :: ApiKey
  , clientRetryPolicy :: RetryPolicy
  , clientBaseUrl :: String
  -- Note: HTTP Manager will be added in Phase 3
  } deriving (Show, Generic)
