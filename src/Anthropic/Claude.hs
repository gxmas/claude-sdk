{- |
Module      : Anthropic.Claude
Description : Prelude and main entry point for Claude SDK
Copyright   : (c) 2026 Geoffrey Noël
License     : MIT
Stability   : experimental

Main entry point for the Claude SDK. Import this module to get
access to all public types, operations, and helpers.

= Basic Usage

@
import Anthropic.Claude

main :: IO ()
main = do
  -- Create a client environment with your API key
  env <- mkClientEnv (ApiKey "sk-ant-...")

  -- Build a simple request
  let req = mkRequest claude4Sonnet [userMsg "Hello!"] 1024

  -- Send the request
  result <- createMessage env req

  -- Handle the response
  case result of
    Left err -> print err
    Right resp -> print (extractText (apiResponseBody resp))
@

= Advanced Features

== Tool Use

@
-- Define a tool
let weatherTool = defineTool "get_weather" "Get weather for a location"
      [ required "location" (withDescription "City name" stringSchema) ]

-- Request with tool
let req = (mkRequest claude4Sonnet [userMsg "What's the weather in SF?"] 1024)
      { requestTools = Just [weatherTool]
      , requestToolChoice = Just AutoChoice
      }
@

== Streaming

@
import Streaming.Prelude qualified as S

forEachEvent env req $ \\evt -> case evt of
  ContentBlockDelta payload ->
    case contentBlockDeltaDelta payload of
      TextDelta txt -> putStr (T.unpack txt)
      ThinkingDelta thinking -> putStr (T.unpack thinking)
      _ -> pure ()
  _ -> pure ()
@

== Retry and Observability

@
-- Configure retry policy
let customRetry = RetryPolicy 5 (ExponentialBackoff 2.0 120.0)
    env' = env { clientRetryPolicy = customRetry }

-- Add event handler for monitoring
let handler evt = print evt  -- Log all events
    env'' = withEventHandler handler env'

-- Enable debug logging
let env''' = withLogger debugLogger env''
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

import Anthropic.Claude.Batch
import Anthropic.Claude.Client
import Anthropic.Claude.Messages
import Anthropic.Claude.Streaming
import Anthropic.Claude.Tools
import Anthropic.Claude.Types
