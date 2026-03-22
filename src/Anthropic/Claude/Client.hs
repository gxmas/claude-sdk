{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Anthropic.Claude.Client
Description : Client convenience functions and configuration
Copyright   : (c) 2026 Anthropic
License     : MIT
Maintainer  : gnoel@anthropic.com

High-level client functions for working with the Claude API.
Provides retry policy management and client environment utilities.
-}
module Anthropic.Claude.Client
  ( -- * Client Environment
    mkClientEnv
  , defaultClientEnv

    -- * Retry Policy Management
  , withRetryPolicy

    -- * Re-exports from Types.Client
  , module Anthropic.Claude.Types.Client
  ) where

import Anthropic.Claude.Internal.HTTP (mkClientEnv, defaultClientEnv)
import Anthropic.Claude.Internal.Retry (withRetryPolicy)
import Anthropic.Claude.Types.Client
