{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Anthropic.Claude.Internal.Retry
Description : Retry logic with exponential backoff
Copyright   : (c) 2026 Anthropic
License     : MIT
Stability   : internal

Retry logic implementation per ADR 0003. Provides configurable retry
policies with exponential backoff for transient API errors.

Retryable errors (per ADR 0003):
- 429 Rate Limit - Always retry with backoff
- 500 Server Error - Retry (transient failure)
- 529 Overloaded - Retry with longer backoff

Non-retryable errors (permanent failures):
- 400 Invalid Request
- 401 Authentication Error
- 403 Permission Error
- 404 Not Found
-}
module Anthropic.Claude.Internal.Retry
  ( -- * Retry Execution
    withRetry
  , withRetryPolicy

    -- * Backoff Calculation
  , calculateBackoff
  , shouldRetryError
  ) where

import Anthropic.Claude.Types.Client
import Anthropic.Claude.Types.Error
import Anthropic.Claude.Types.Logging (logRetryAttempt, logApiError)
import Anthropic.Claude.Types.Observability
import Control.Concurrent (threadDelay)
import Data.Time.Clock (NominalDiffTime)
import UnliftIO (MonadUnliftIO, liftIO)

-- | Execute an action with retry logic
--
-- Per ADR 0003, retries are only attempted for transient errors:
-- - 429 Rate Limit
-- - 500 Server Error
-- - 529 Overloaded
--
-- Uses the backoff strategy from the 'ClientEnv' retry policy to
-- calculate delays. Emits 'RetryEvent' via the event handler before
-- each retry delay.
--
-- Example:
-- @
-- result <- withRetry env $ createMessage env req
-- @
withRetry
  :: MonadUnliftIO m
  => ClientEnv
  -> m (Either APIError a)
  -> m (Either APIError a)
withRetry env action = go 0
  where
    policy = clientRetryPolicy env
    handler = clientEventHandler env
    logSettings = clientLogSettings env
    maxAttempts = retryMaxAttempts policy

    go attempt
      | attempt > maxAttempts = action  -- Exceeded max attempts, run once more without retry
      | otherwise = do
          result <- action
          case result of
            Right success -> pure $ Right success
            Left err
              | shouldRetryError err && attempt < maxAttempts -> do
                  let delay = calculateBackoff policy attempt
                  liftIO $ do
                    emitEvent handler $ Retry RetryEvent
                      { retryEvtError       = err
                      , retryEvtAttempt     = attempt + 1
                      , retryEvtMaxAttempts = maxAttempts
                      , retryEvtBackoff     = delay
                      }
                    logRetryAttempt logSettings err (attempt + 1) maxAttempts delay
                    threadDelay (microSeconds delay)
                  go (attempt + 1)
              | otherwise -> do
                  liftIO $ logApiError logSettings err
                  pure $ Left err

    -- Convert NominalDiffTime to microseconds for threadDelay
    microSeconds :: NominalDiffTime -> Int
    microSeconds dt = round (dt * 1000000)

-- | Check if an error should trigger a retry
--
-- Uses the isRetryable function from Types.Error module.
shouldRetryError :: APIError -> Bool
shouldRetryError = isRetryable

-- | Calculate backoff delay for a given attempt
--
-- Implements the backoff strategy from RetryPolicy:
--
-- * 'ExponentialBackoff': @min maxDelay (baseDelay * 2^attempt)@
-- * 'ConstantBackoff': Fixed delay
-- * 'NoBackoff': Zero delay
--
-- Examples:
-- @
-- -- Exponential: attempt 0 = 1s, attempt 1 = 2s, attempt 2 = 4s, ...
-- let policy = RetryPolicy 3 (ExponentialBackoff 1.0 60.0)
-- calculateBackoff policy 0  -- 1.0 seconds
-- calculateBackoff policy 1  -- 2.0 seconds
-- calculateBackoff policy 2  -- 4.0 seconds
-- calculateBackoff policy 5  -- 32.0 seconds (capped at 60.0)
-- @
calculateBackoff :: RetryPolicy -> Int -> NominalDiffTime
calculateBackoff (RetryPolicy _ strategy) attempt =
  case strategy of
    ExponentialBackoff base maxDelay ->
      min maxDelay (base * (2 ^ attempt))
    ConstantBackoff delay ->
      delay
    NoBackoff ->
      0

-- | Apply a retry policy to an action
--
-- Convenience combinator that modifies the retry policy for a single operation.
--
-- Example:
-- @
-- result <- withRetryPolicy aggressiveRetryPolicy $ \env ->
--   createMessage env req
-- @
withRetryPolicy
  :: RetryPolicy
  -> (ClientEnv -> m (Either APIError a))
  -> ClientEnv
  -> m (Either APIError a)
withRetryPolicy policy action env =
  let env' = env { clientRetryPolicy = policy }
  in action env'
