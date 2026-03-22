# claude-sdk

A production-grade Haskell SDK for the [Claude API](https://docs.anthropic.com/en/docs).

## Features

- **Type-safe API** — discriminated unions, newtypes, and exhaustive pattern matching
- **Streaming** — Server-Sent Events with `Either APIError StreamEvent` for partial error recovery
- **Retry logic** — configurable exponential backoff for transient errors (429, 500, 529)
- **Batch API** — create, retrieve, list, cancel, and poll batches
- **Tool use** — helpers for defining tools, extracting calls, and building results
- **Rate limit tracking** — `APIResponse` wrapper carries rate limit metadata
- **MonadUnliftIO** — works in `IO`, `ReaderT`, and custom monad stacks

## Installation

Add to your `build-depends` in your `.cabal` file:

```cabal
build-depends: claude-sdk
```

Or with Stack, add to `extra-deps` in `stack.yaml`.

## Quick Start

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Anthropic.Claude

main :: IO ()
main = do
  env <- mkClientEnv (ApiKey "sk-ant-...")
  let req = mkRequest claude4Sonnet [userMsg "What is 2+2?"] 1024
  result <- createMessage env req
  case result of
    Left err  -> print err
    Right resp -> putStrLn $ show $ extractText (apiResponseBody resp)
```

## API Overview

### Messages

```haskell
-- Simple message
let req = mkRequest claude4Sonnet [userMsg "Hello!"] 1024
result <- createMessage env req

-- With system prompt and temperature
let req = (mkRequest claude4Sonnet [userMsg "Write a haiku"] 1024)
      { requestSystem = Just "You are a poet."
      , requestTemperature = Just 0.9
      }
```

### Streaming

```haskell
-- Bracket-style resource safety
withMessageStream env req $ \stream -> do
  finalMsg <- S.mapM_ handleEvent stream
  print (responseUsage finalMsg)

-- Simple callback style
finalMsg <- forEachEvent env req $ \evt ->
  case evt of
    ContentBlockDelta payload ->
      case contentBlockDeltaDelta payload of
        TextDelta txt -> putStr (T.unpack txt)
        _ -> pure ()
    _ -> pure ()
```

### Tool Use

```haskell
-- Define a tool
let weatherTool = defineTool
      "get_weather"
      "Get weather for a location"
      weatherSchema

-- Extract tool calls from response
let calls = extractToolCalls response
forM_ calls $ \(toolId, name, input) -> do
  result <- executeMyTool name input
  let resultBlock = buildToolResult toolId result
  -- Send result back to Claude...
```

### Batch API

```haskell
-- Create a batch
let batchReq = CreateBatchRequest
      [ BatchRequest "req-1" (mkRequest claude4Sonnet [userMsg "Hi"] 100)
      , BatchRequest "req-2" (mkRequest claude4Sonnet [userMsg "Hello"] 100)
      ]
result <- createBatch env batchReq

-- Poll until done
case result of
  Right resp -> do
    let batchId = batchResponseId (apiResponseBody resp)
    finalBatch <- pollBatchUntilDone env batchId 10.0
    print finalBatch
```

### Retry Policies

```haskell
-- Default: 3 retries, exponential backoff 1s–60s
env <- mkClientEnv apiKey  -- uses defaultRetryPolicy

-- Per-request override
result <- withRetryPolicy aggressiveRetryPolicy (\e -> createMessage e req) env

-- Available presets
defaultRetryPolicy      -- 3 retries, 1s–60s exponential
aggressiveRetryPolicy   -- 5 retries, 0.5s–120s exponential
noRetries               -- fail fast
```

### Error Handling

```haskell
result <- createMessage env req
case result of
  Left err -> do
    -- API errors (4xx, 5xx) are in Either
    case errorKind err of
      RateLimitError -> putStrLn "Rate limited, retry later"
      AuthenticationError -> putStrLn "Invalid API key"
      _ -> print err
  Right resp -> do
    -- Success: response + metadata
    let msg = apiResponseBody resp
    print (extractText msg)
    -- Rate limit info from headers
    print (apiResponseRateLimitInfo resp)
```

## Architecture

The SDK follows a layered module architecture:

| Tier | Modules | Purpose |
|------|---------|---------|
| 1 | `Anthropic.Claude` | Prelude — re-exports everything |
| 2 | `Messages`, `Streaming`, `Batch`, `Tools`, `Client` | Public operations |
| 3 | `Types.*` | Type definitions (Core, Common, Error, Client, Request, Response, Stream, Batch) |
| 4 | `Internal.*` | Implementation details (JSON, HTTP, Retry, SSE, Streaming) |

## Design Decisions

See the [Architecture Decision Records](adr/) for rationale:

- **ADR 0001**: MonadUnliftIO for effect system
- **ADR 0002**: Hybrid error handling (exceptions for network, Either for API)
- **ADR 0003**: Retry policy with per-request override
- **ADR 0004**: APIResponse wrapper for rate limit metadata
- **ADR 0005**: Either in stream for partial error recovery
- **ADR 0006**: streaming library, aeson, http-client
- **ADR 0007**: 4-tier module structure

## Requirements

- GHC 9.6 or later
- Cabal 3.0 or later

## License

MIT — see [LICENSE](LICENSE).
