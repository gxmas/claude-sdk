{-|
Module      : Anthropic.Claude
Description : Prelude and main entry point for Claude SDK
Copyright   : (c) 2026 Geoffrey Noël
License     : MIT
Stability   : experimental

Main entry point for the Claude SDK. Import this module to get
access to all public types, operations, and helpers.

@
import Anthropic.Claude

main :: IO ()
main = do
  env <- mkClientEnv (ApiKey "sk-ant-...")
  let req = mkRequest claude4Sonnet [userMsg "Hello!"] 1024
  result <- createMessage env req
  case result of
    Left err -> print err
    Right resp -> print (extractText (apiResponseBody resp))
@
-}
module Anthropic.Claude
  ( -- * Types
    module Anthropic.Claude.Types

    -- * Client
  , module Anthropic.Claude.Client

    -- * Messages API
  , module Anthropic.Claude.Messages

    -- * Streaming API
  , module Anthropic.Claude.Streaming

    -- * Batch API
  , module Anthropic.Claude.Batch

    -- * Tool Use Helpers
  , module Anthropic.Claude.Tools
  ) where

import Anthropic.Claude.Types
import Anthropic.Claude.Client
import Anthropic.Claude.Messages
import Anthropic.Claude.Streaming
import Anthropic.Claude.Batch
import Anthropic.Claude.Tools
