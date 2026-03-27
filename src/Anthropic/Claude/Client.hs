{- |
Module      : Anthropic.Claude.Client
Description : Client convenience functions and configuration
Copyright   : (c) 2026 Geoffrey Noël
License     : MIT
Maintainer  : noel.geoff@gmail.com

High-level client functions for working with the Claude API.
Provides retry policy management, event handler configuration,
logging, and client environment utilities.
-}
module Anthropic.Claude.Client
  ( -- * Client Environment
    mkClientEnv
  , defaultClientEnv

    -- * Retry Policy Management
  , withRetryPolicy

    -- * Observability
  , withEventHandler

    -- * Logging
  , withLogger
  , withLogBodyLimit

    -- * Re-exports from Types.Client
  , module Anthropic.Claude.Types.Client
  ) where

import Anthropic.Claude.Internal.HTTP (defaultClientEnv, mkClientEnv)
import Anthropic.Claude.Internal.Retry (withRetryPolicy)
import Anthropic.Claude.Types.Client
import Anthropic.Claude.Types.Logging (LogSettings (..), Logger, defaultLogSettings)
import Anthropic.Claude.Types.Observability (EventHandler)

-- | Set the event handler for observability.
--
-- The handler will receive API events at request/response
-- lifecycle points. Use @combineHandlers@ to fan out to multiple
-- observers.
--
-- Example:
-- @
-- let handler evt = putStrLn $ "Event: " ++ show evt
-- env' <- withEventHandler handler <$> mkClientEnv key
-- @
withEventHandler :: EventHandler -> ClientEnv -> ClientEnv
withEventHandler h env = env {clientEventHandler = Just h}

-- | Enable debug logging with the given logger.
--
-- Uses a default body limit of 4096 bytes. Use 'withLogBodyLimit'
-- to change the limit.
--
-- Example:
-- @
-- env <- withLogger debugLogger <$> mkClientEnv key
-- -- or with level filtering:
-- env <- withLogger (stderrLogger LevelInfo) <$> mkClientEnv key
-- @
withLogger :: Logger -> ClientEnv -> ClientEnv
withLogger l env = env {clientLogSettings = Just (defaultLogSettings l)}

-- | Set the maximum body size (in bytes) included in debug log output.
--
-- Only has effect if a logger is configured via 'withLogger'.
-- Default is 4096 bytes.
--
-- Example:
-- @
-- env <- withLogBodyLimit 8192 . withLogger debugLogger <$> mkClientEnv key
-- @
withLogBodyLimit :: Int -> ClientEnv -> ClientEnv
withLogBodyLimit n env =
  env
    { clientLogSettings = fmap (\s -> s {logBodyLimit = n}) (clientLogSettings env)
    }
