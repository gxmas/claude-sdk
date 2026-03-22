# Haskell Claude SDK Implementation Plan

## Context

We are implementing a production-grade Haskell SDK for the Claude API from scratch. The design is fully specified in 7 Architecture Decision Records (ADRs) located at `/Users/gnoel5/Projects/design/anthropic/adr/`, with complete API specifications in `/Users/gnoel5/Projects/design/anthropic/docs/sdk-reference/`.

**Why this implementation?** The current codebase is empty (only `CLAUDE.md`). We need to build a complete, production-ready SDK that follows the approved architecture.

**Key architectural decisions:**
- **Effect system**: `MonadUnliftIO` constraint for streaming and concurrency (ADR 0001)
- **Error handling**: Network errors as exceptions, API errors in `Either` (ADR 0002)
- **Retry policy**: Default in `ClientEnv` + per-request override (ADR 0003)
- **Rate limits**: `APIResponse` wrapper type carrying metadata (ADR 0004)
- **Streaming**: `Either APIError StreamEvent` in stream with `throwOnError` combinator (ADR 0005)
- **Tech stack**: `http-client`, `aeson`, `streaming` library, `hspec`, Cabal (ADR 0006)
- **21 modules** in 4 tiers: Prelude, Operations, Types, Internal (ADR 0007)

**Scope**: 27 data types, Messages API, Streaming (SSE), Batch operations, Tool use helpers

---

## Implementation Approach

**Strategy**: Bottom-up, depth-first, test-as-you-go

- **Bottom-up**: Types → HTTP infrastructure → Operations (follows dependency graph)
- **Depth-first**: Complete vertical slices (e.g., Phase 2 delivers working `createMessage`)
- **Test-as-you-go**: Property-based tests validate each phase before proceeding

This approach validates the design early, catches bugs cheaply, and delivers demonstrable milestones.

---

## Phase 1: Project Skeleton (Day 1) - Buildable & Testable Foundation

**Goal**: Create a buildable, testable Haskell project skeleton

**Why first?** Establishes the build system and test infrastructure before writing any code. After this phase, `cabal build` and `cabal test` work for all remaining development.

### Deliverables

1. **Initialize cabal project**:
   ```bash
   cd /Users/gnoel5/Projects/probe/claude-sdk
   cabal init --lib --package-name=claude-sdk --license=MIT
   ```

2. **Configure `claude-sdk.cabal`**:

   **Library section**:
   ```cabal
   library
     exposed-modules:
       Anthropic.Claude
       Anthropic.Claude.Types
     other-modules:
       Anthropic.Claude.Types.Core
       Anthropic.Claude.Internal.JSON
     hs-source-dirs: src
     build-depends:
       base >=4.16 && <5,
       aeson >=2.1,
       text >=1.2,
       bytestring >=0.11
     default-language: Haskell2010
     ghc-options: -Wall
   ```

   **Test suite**:
   ```cabal
   test-suite claude-sdk-test
     type: exitcode-stdio-1.0
     main-is: Spec.hs
     hs-source-dirs: test
     build-tool-depends: hspec-discover:hspec-discover ==2.*
     build-depends:
       base,
       claude-sdk,
       hspec >=2.10,
       QuickCheck >=2.14,
       aeson,
       text,
       bytestring
     default-language: Haskell2010
     ghc-options: -Wall -threaded
   ```

3. **Create directory structure**:
   ```bash
   mkdir -p src/Anthropic/Claude/{Types,Internal}
   mkdir -p test/Anthropic/Claude/{Types,Internal}
   ```

4. **Create `test/Spec.hs`** (hspec-discover entry point):
   ```haskell
   {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
   ```

5. **Create stub modules** (empty, just to make the project buildable):

   **`src/Anthropic/Claude.hs`**:
   ```haskell
   module Anthropic.Claude () where
   ```

   **`src/Anthropic/Claude/Types.hs`**:
   ```haskell
   module Anthropic.Claude.Types () where
   ```

6. **Create `.gitignore`**:
   ```
   dist-newstyle/
   .ghc.environment.*
   *.hi
   *.o
   ```

### Validation

- [ ] `cabal build` succeeds (compiles empty library)
- [ ] `cabal test` succeeds (runs hspec-discover with no tests)
- [ ] Project structure matches plan:
  ```
  claude-sdk/
    src/Anthropic/Claude/{Types,Internal}/  (exist)
    test/Anthropic/Claude/{Types,Internal}/  (exist)
    test/Spec.hs                             (contains hspec-discover pragma)
    claude-sdk.cabal                         (library + test suite configured)
  ```
- [ ] Can run `cabal repl` and load the library
- [ ] Zero warnings from `cabal build`

**Time estimate**: 1-2 hours (Day 1 morning)

**After this phase, `cabal build` and `cabal test` work for all remaining phases.**

---

## Phase 2: Foundation (Week 1) - JSON Codecs + Core Types

**Goal**: Establish type foundation with validated JSON serialization

**Why now?** Types are the dependency root. JSON codecs can be tested in isolation without HTTP.

### Modules to Implement (5 source + 4 test modules, ~630 LOC)

1. **`src/Anthropic/Claude/Types/Core.hs`** (~80 LOC)
   - Newtypes: `ApiKey`, `ModelId`, `RequestId`, `MessageId`, `BatchId`, `ToolCallId`
   - Enums: `Role` (User, Assistant), `StopReason` (EndTurn, MaxTokens, StopSequence, ToolUse)
   - Model constants: `claude-opus-4-6`, `claude-sonnet-4-6`, `claude-haiku-4-5`

2. **`src/Anthropic/Claude/Types/Common.hs`** (~120 LOC) ⭐ **Critical**
   - `ContentBlock` discriminated union (TextBlock, ImageBlock, ToolUseBlock, ToolResultBlock)
   - `ImageSource` discriminated union (Base64Source, URLSource)
   - `CacheControl`, `MessageContent`
   - **Sets pattern for all discriminated union JSON instances**

3. **`src/Anthropic/Claude/Types/Error.hs`** (~100 LOC)
   - `APIError` with 7 variants (InvalidRequest, AuthenticationError, RateLimitError, etc.)
   - `APIErrorKind` enum, `ErrorDetails` record
   - `NetworkError` exception type

4. **`src/Anthropic/Claude/Internal/JSON.hs`** (~150 LOC) ⭐ **Critical**
   - Shared `aesonOptions` (camelCase ↔ snake_case, omitNothingFields)
   - `parseDiscriminated` helper for `type` field dispatch
   - **All other modules depend on these utilities**

5. **`src/Anthropic/Claude/Types/Client.hs`** (~100 LOC)
   - `ClientEnv` (opaque type, constructor hidden)
   - `RetryPolicy`, `BackoffStrategy` (ExponentialBackoff | ConstantBackoff | NoBackoff)
   - `RateLimitInfo` record

### Testing (4 test modules, ~300 LOC)

Following standard Haskell conventions, tests mirror the source module structure with `*Spec.hs` files:

6. **`test/Anthropic/Claude/Types/CoreSpec.hs`** (~60 LOC)
   - QuickCheck generators: `Arbitrary` instances for `ApiKey`, `ModelId`, `Role`, `StopReason`
   - Properties: JSON roundtrip, enum encoding correctness
   - Unit tests: Model constants validate

7. **`test/Anthropic/Claude/Types/CommonSpec.hs`** (~120 LOC) ⭐ **Critical**
   - Generators for `ContentBlock`, `ImageSource` (all variants)
   - Properties: JSON roundtrip, discriminator dispatch (`type` field), field naming (snake_case)
   - Unit tests: Example-based parsing for each ContentBlock variant
   - **Sets testing pattern for all discriminated unions**

8. **`test/Anthropic/Claude/Types/ErrorSpec.hs`** (~60 LOC)
   - Generators for `APIError` (all 7 kinds)
   - Properties: JSON roundtrip, error kind mapping

9. **`test/Anthropic/Claude/Types/ClientSpec.hs`** (~60 LOC)
   - Generators for `RetryPolicy`, `BackoffStrategy`
   - Properties: Backoff strategy serialization

10. **`test/Spec.hs`** (auto-discovery)
    ```haskell
    {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
    ```
    - Discovers all `*Spec.hs` files automatically

### Key Files from Design
- `/Users/gnoel5/Projects/design/anthropic/docs/sdk-reference/01-type-system.md` - Complete type specifications
- `/Users/gnoel5/Projects/design/anthropic/adr/0006-tech-stack.md` - JSON derivation strategy

### Validation

**Buildability**:
- [ ] `cabal build` succeeds (compiles all Phase 2 source modules)
- [ ] `cabal test` runs and passes all Phase 2 tests
- [ ] Zero compilation warnings (`-Wall` enabled)

**Tests** (4 test modules, ~400 test cases):
- [ ] `Types/CoreSpec.hs` - Core types roundtrip, enums encode correctly
- [ ] `Types/CommonSpec.hs` - ContentBlock/ImageSource roundtrip, discriminator dispatch
- [ ] `Types/ErrorSpec.hs` - APIError roundtrip, all 7 error kinds
- [ ] `Types/ClientSpec.hs` - RetryPolicy/BackoffStrategy roundtrip

**Property tests** (run with `--test-show-details=direct`):
- [ ] JSON roundtrip: 100 examples per type (Core: 6 types, Common: 2, Error: 1, Client: 2) ≈ 1,100 QuickCheck cases
- [ ] Discriminated unions: `type` field matches variant
- [ ] Field naming: camelCase (Haskell) ↔ snake_case (JSON)

**Documentation**:
- [ ] All public types have Haddock comments
- [ ] Module headers document purpose

**Time estimate**: 5 days

---

## Phase 3: HTTP Infrastructure (Week 2) - Client + Messages API

**Goal**: Working HTTP client that can make authenticated API requests

**Why next?** Enables actual API calls. Keeps scope minimal (no retry/streaming yet).

### Modules to Implement (4 modules, ~900 LOC)

1. **`src/Anthropic/Claude/Types/Request.hs`** (~250 LOC)
   - `CreateMessageRequest` record with all fields
   - `Tool` record, `ToolChoice` discriminated union
   - Helper constructors: `userMsg`, `assistantMsg`, `assistantMsgBlocks`
   - JSON instances with validation (non-empty messages, max_tokens > 0)

2. **`src/Anthropic/Claude/Types/Response.hs`** (~200 LOC)
   - `MessageResponse` record, `Usage` record (token counts)
   - `APIResponse a` wrapper (carries rate limit info + request ID)
   - Helper functions: `extractText`, `unwrapOrThrow`

3. **`src/Anthropic/Claude/Internal/HTTP.hs`** (~300 LOC) ⭐ **Critical**
   - `ClientEnv` data type (full constructor, hidden from public API)
   - `mkClientEnv :: ApiKey -> IO ClientEnv` (creates TLS manager with connection pooling)
   - `buildRequest :: ClientEnv -> Method -> ByteString -> RequestBody -> Request`
   - Sets headers: `x-api-key`, `anthropic-version`, `content-type`, `user-agent`
   - `parseResponse :: Response ByteString -> Either APIError (APIResponse MessageResponse)`
   - **Foundation for all HTTP operations**

4. **`src/Anthropic/Claude/Messages.hs`** (~150 LOC)
   - `createMessage :: MonadUnliftIO m => ClientEnv -> CreateMessageRequest -> m (Either APIError (APIResponse MessageResponse))`
   - Response parsing: status code → error variant mapping, rate limit header extraction

### Testing (4 test modules, ~400 LOC)

5. **`test/Anthropic/Claude/Types/RequestSpec.hs`** (~100 LOC)
   - Generators for `CreateMessageRequest`, `Tool`, `ToolChoice`
   - Properties: JSON roundtrip, field validation (non-empty messages, max_tokens > 0)
   - Unit tests: Helper constructors (`userMsg`, `assistantMsg`)

6. **`test/Anthropic/Claude/Types/ResponseSpec.hs`** (~80 LOC)
   - Generators for `MessageResponse`, `APIResponse`, `Usage`
   - Properties: JSON roundtrip
   - Unit tests: `extractText`, `unwrapOrThrow` helper functions

7. **`test/Anthropic/Claude/Internal/HTTPSpec.hs`** (~120 LOC) ⭐ **Critical**
   - Mock HTTP dispatch: `mockDispatch :: [(Request -> Bool, Response ByteString)] -> Request -> IO (Response ByteString)`
   - Unit tests: `buildRequest` sets correct headers (`x-api-key`, `anthropic-version`)
   - Unit tests: `parseResponse` maps status codes to `APIError` variants (400→InvalidRequest, 401→AuthenticationError, etc.)
   - Unit tests: Rate limit header extraction into `RateLimitInfo`
   - Integration: `mkClientEnv` creates working TLS manager

8. **`test/Anthropic/Claude/MessagesSpec.hs`** (~100 LOC)
   - Integration tests with mock HTTP:
     - `createMessage` sends correct POST request
     - Successful 200 response parses to `MessageResponse`
     - Error responses (400, 401, 429, 500) return correct `APIError`
     - Rate limit metadata preserved in `APIResponse`

### Key Files from Design
- `/Users/gnoel5/Projects/design/anthropic/docs/sdk-reference/02-client-architecture.md` - Client architecture
- `/Users/gnoel5/Projects/design/anthropic/docs/sdk-reference/03-request-construction.md` - Request building
- `/Users/gnoel5/Projects/design/anthropic/adr/0004-rate-limit-response-wrapper.md` - APIResponse design

### Validation
- [ ] `cabal test` passes all Phase 2 tests:
  - `Types/RequestSpec.hs` - Request types roundtrip, validation, helpers
  - `Types/ResponseSpec.hs` - Response types roundtrip, helper functions
  - `Internal/HTTPSpec.hs` - Request building, header setting, response parsing
  - `MessagesSpec.hs` - `createMessage` integration with mock HTTP
- [ ] Mock HTTP tests verify:
  - `mkClientEnv` creates TLS manager with connection pooling
  - `buildRequest` sets correct headers (`x-api-key`, `anthropic-version`)
  - Status codes map to correct `APIError` variants (400→InvalidRequest, etc.)
  - Rate limit headers extract into `RateLimitInfo`
- [ ] Manual test: `createMessage` with real API key returns valid response

**Time estimate**: 5 days

---

## Phase 4: Resilience (Week 3) - Retry + Error Handling

**Goal**: Production-ready retry logic and error handling

**Why now?** Core reliability features before adding streaming complexity.

### Modules to Implement (2 modules, ~300 LOC)

1. **`src/Anthropic/Claude/Internal/Retry.hs`** (~200 LOC)
   - `withRetry :: MonadUnliftIO m => RetryPolicy -> m (Either APIError a) -> m (Either APIError a)`
   - `shouldRetry :: APIError -> Bool` (retryable: 429, 500, 529)
   - `calculateBackoff :: RetryPolicy -> Int -> NominalDiffTime`
   - Exponential backoff: `min maxDelay (baseDelay * 2^attempt)`

2. **`src/Anthropic/Claude/Client.hs`** (~100 LOC)
   - `withRetryPolicy :: RetryPolicy -> ClaudeM a -> ClaudeM a`
   - Convenience retry policies: `noRetries`, `defaultRetryPolicy`, `aggressiveRetryPolicy`

3. **Update `Anthropic.Claude.Messages`**
   - Integrate retry into `createMessage`: `withRetry (env ^. retryPolicy) $ ...`

### Testing (2 test modules, ~200 LOC)

4. **`test/Anthropic/Claude/Internal/RetrySpec.hs`** (~120 LOC)
   - Properties:
     - `prop_retry_respects_max_attempts` - verify retry count limit with `IORef` counter
     - `prop_backoff_increases_exponentially` - verify `calculateBackoff` formula
     - `prop_non_retryable_errors_fail_immediately` - verify no retries for 400, 401
     - `prop_retryable_errors_trigger_retries` - verify retries for 429, 500, 529
   - Unit tests: `shouldRetry` for each `APIError` kind

5. **`test/Anthropic/Claude/ClientSpec.hs`** (~80 LOC)
   - Unit tests: `withRetryPolicy` modifies ClientEnv correctly
   - Unit tests: Convenience policies (`noRetries`, `defaultRetryPolicy`, `aggressiveRetryPolicy`)
   - Integration: Retry behavior with mock HTTP (inject transient failures)

### Key Files from Design
- `/Users/gnoel5/Projects/design/anthropic/adr/0003-retry-policy-combinator-override.md` - Retry design
- `/Users/gnoel5/Projects/design/anthropic/docs/sdk-reference/05-error-handling.md` - Error handling strategy

### Validation
- [ ] `cabal test` passes all Phase 3 tests:
  - `Internal/RetrySpec.hs` - Retry logic properties (backoff, max attempts, error classification)
  - `ClientSpec.hs` - Client combinators (`withRetryPolicy`), convenience policies
- [ ] Property tests verify:
  - Exponential backoff formula: `min maxDelay (baseDelay * 2^attempt)`
  - Max retry count respected (via `IORef` counter)
  - Retryable errors (429, 500, 529) trigger retries
  - Non-retryable errors (400, 401) fail immediately
- [ ] Integration tests: Mock HTTP with transient failures shows retry behavior
- [ ] Manual test: Send request with aggressive retry policy to real API

**Time estimate**: 4 days

---

## Phase 5: Streaming (Week 4-5) - SSE + Streaming API

**Goal**: Working streaming API with Server-Sent Events (SSE) parsing

**Why now?** Most complex feature, benefits from solid foundation (retry, error handling).

### Modules to Implement (4 modules, ~850 LOC)

1. **`src/Anthropic/Claude/Types/Stream.hs`** (~200 LOC)
   - `StreamEvent` discriminated union (8 variants: MessageStart, ContentBlockStart, ContentBlockDelta, etc.)
   - `ContentDelta` (TextDelta | InputJsonDelta)
   - `MessageDelta`, `MessageStartEvent`, etc.
   - JSON instances with `type` field dispatch

2. **`src/Anthropic/Claude/Internal/SSE.hs`** (~250 LOC) ⭐ **Critical**
   - `data SSEEvent = SSEEvent { eventType :: Maybe Text, eventData :: ByteString }`
   - `parseSSELine :: ByteString -> Maybe (Either SSEField SSEEvent)`
   - `parseSSE :: MonadIO m => BodyReader -> m (Stream (Of SSEEvent) m ())`
   - Handles multi-line data, event types, comments, ping events
   - **Most complex parsing logic in the SDK**

3. **`src/Anthropic/Claude/Internal/Streaming.hs`** (~200 LOC)
   - `type MessageStream m = Stream (Of StreamEvent) m MessageResponse`
   - `createMessageStream :: MonadUnliftIO m => ClientEnv -> CreateMessageRequest -> m (Stream (Of (Either APIError StreamEvent)) m MessageResponse)`
   - Parses SSE events, decodes JSON, yields `Either APIError StreamEvent`
   - Return type `r = MessageResponse` carries finalized message (ADR 0006)

4. **`src/Anthropic/Claude/Streaming.hs`** (~200 LOC)
   - `withMessageStream :: MonadUnliftIO m => ClientEnv -> CreateMessageRequest -> (MessageStream m -> m a) -> m a`
   - `forEachEvent :: MonadUnliftIO m => ClientEnv -> CreateMessageRequest -> (StreamEvent -> m ()) -> m MessageResponse`
   - `throwOnError :: MonadThrow m => Stream (Of (Either e a)) m r -> Stream (Of a) m r`
   - Wrapped combinators: `foldStream`, `mapStreamM_`, `takeStream`, `filterStream`

### Testing (4 test modules, ~400 LOC)

5. **`test/Anthropic/Claude/Types/StreamSpec.hs`** (~80 LOC)
   - Generators for `StreamEvent` (all 8 variants), `ContentDelta`
   - Properties: JSON roundtrip, discriminated dispatch on `type` field
   - Unit tests: Example-based parsing for each event type

6. **`test/Anthropic/Claude/Internal/SSESpec.hs`** (~120 LOC) ⭐ **Critical**
   - Properties:
     - `prop_sse_line_parsing` - arbitrary SSE lines parse without crashing
     - `prop_multiline_data_concatenation` - multi-line `data:` fields concatenate correctly
   - Unit tests:
     - Single-line events (`event: message_start\ndata: {...}\n\n`)
     - Multi-line data (`data: line1\ndata: line2\n\n`)
     - Comments (`# comment\n`)
     - Ping events (`event: ping\n\n`)
     - Empty lines (event delimiters)
   - **Most critical parser tests in SDK**

7. **`test/Anthropic/Claude/Internal/StreamingSpec.hs`** (~100 LOC)
   - Integration with mock SSE responses:
     - `createMessageStream` parses SSE → JSON → StreamEvent
     - Error events yield `Left APIError`
     - Stream return type carries finalized `MessageResponse`
   - Properties: `prop_partial_stream_recovery` - accumulate events before error

8. **`test/Anthropic/Claude/StreamingSpec.hs`** (~100 LOC)
   - Integration tests:
     - `withMessageStream` success: all events yielded, finalized message returned
     - `forEachEvent` callback receives all events in order
     - `throwOnError` converts `Either` stream to plain stream (throws on Left)
     - Resource cleanup: HTTP response closed even on exception
   - Unit tests: Wrapped combinators (`foldStream`, `mapStreamM_`, `takeStream`, `filterStream`)

### Key Files from Design
- `/Users/gnoel5/Projects/design/anthropic/docs/sdk-reference/06-streaming.md` - SSE streaming spec
- `/Users/gnoel5/Projects/design/anthropic/adr/0005-streaming-errors-either-in-stream.md` - Stream error design
- `/Users/gnoel5/Projects/design/anthropic/adr/0006-tech-stack.md` - `streaming` library choice

### Validation
- [ ] `cabal test` passes all Phase 4 tests:
  - `Types/StreamSpec.hs` - Stream event types roundtrip, discriminated dispatch
  - `Internal/SSESpec.hs` - SSE line parsing, multi-line data, edge cases
  - `Internal/StreamingSpec.hs` - Stream creation, error events, return type
  - `StreamingSpec.hs` - Public API (`withMessageStream`, `forEachEvent`, combinators)
- [ ] SSE parser tests verify:
  - Single-line events parse correctly
  - Multi-line `data:` fields concatenate
  - Comments and ping events handled
  - Arbitrary bytes don't crash parser (property test)
- [ ] Streaming integration tests verify:
  - `withMessageStream` brackets HTTP response (resource cleanup)
  - `throwOnError` converts `Either` stream (throws on `Left`)
  - `forEachEvent` callback receives events in order
  - Partial recovery: accumulate events before error
  - Finalized `MessageResponse` with usage stats returned at stream end
- [ ] Manual test: Stream real API response, verify all events received

**Time estimate**: 8 days

---

## Phase 6: Batch + Tools (Week 6-7) - Complete API Coverage

**Goal**: Full Claude API surface including batch operations and tool helpers

**Why last?** Batch and Tools are independent of streaming/messages, can be developed in parallel if needed.

### Modules to Implement (4 modules, ~600 LOC)

1. **`src/Anthropic/Claude/Types/Batch.hs`** (~150 LOC)
   - `CreateBatchRequest`, `BatchResponse`, `BatchResult`
   - `ProcessingStatus` (InProgress | Canceling | Ended)
   - `BatchResultData` (SuccessResult | ErrorResult | ExpiredResult)
   - `RequestCounts` record

2. **`src/Anthropic/Claude/Batch.hs`** (~250 LOC)
   - `createBatch`, `retrieveBatch`, `listBatches`, `cancelBatch`
   - `withBatchResults :: MonadUnliftIO m => ClientEnv -> BatchId -> (Stream (Of BatchResult) m () -> m a) -> m a`
   - `pollBatchUntilDone :: MonadUnliftIO m => ClientEnv -> BatchId -> NominalDiffTime -> m (Either APIError BatchResponse)`
   - JSONL parsing for batch results

3. **`src/Anthropic/Claude/Tools.hs`** (~150 LOC)
   - `defineTool :: Text -> Text -> Value -> Tool` (smart constructor with validation)
   - `extractToolCalls :: MessageResponse -> [ToolUseBlock]`
   - `buildToolResult :: ToolCallId -> Value -> Bool -> ToolResultBlock`

4. **`src/Anthropic/Claude.hs`** (~50 LOC)
   - Prelude module re-exporting all public API
   - Clean import for users: `import Anthropic.Claude`

### Testing (3 test modules, ~250 LOC)

5. **`test/Anthropic/Claude/Types/BatchSpec.hs`** (~60 LOC)
   - Generators for `CreateBatchRequest`, `BatchResponse`, `BatchResult`
   - Properties: JSON roundtrip
   - Unit tests: `ProcessingStatus` enum, `BatchResultData` discriminated union

6. **`test/Anthropic/Claude/BatchSpec.hs`** (~100 LOC)
   - Integration tests with mock HTTP:
     - `createBatch`, `retrieveBatch`, `listBatches`, `cancelBatch` send correct requests
     - `withBatchResults` parses JSONL stream correctly
     - `pollBatchUntilDone` polls with correct interval, stops when `status == Ended`

7. **`test/Anthropic/Claude/ToolsSpec.hs`** (~90 LOC)
   - Unit tests:
     - `defineTool` validates tool schema
     - `extractToolCalls` finds all `ToolUseBlock` in response
     - `buildToolResult` constructs valid `ToolResultBlock`
   - Integration: End-to-end tool use loop
     - Send request with tools → extract calls → execute → send results → final response
   - Error scenarios: Invalid API key, rate limits, malformed requests

### Key Files from Design
- `/Users/gnoel5/Projects/design/anthropic/docs/sdk-reference/07-tool-use.md` - Tool use specification
- `/Users/gnoel5/Projects/design/anthropic/docs/sdk-reference/08-vision-batches.md` - Batch API spec

### Validation
- [ ] `cabal test` passes all Phase 5 tests:
  - `Types/BatchSpec.hs` - Batch types roundtrip, discriminated unions
  - `BatchSpec.hs` - Batch operations, JSONL parsing, polling
  - `ToolsSpec.hs` - Tool helpers, end-to-end tool use loop
- [ ] Batch tests verify:
  - All operations (create, retrieve, list, cancel) send correct requests
  - `withBatchResults` parses JSONL stream correctly
  - `pollBatchUntilDone` polls with interval, stops when `status == Ended`
- [ ] Tool tests verify:
  - `defineTool` validates schema
  - `extractToolCalls` finds all tool use blocks
  - End-to-end: request → extract → execute → results → final response
- [ ] Prelude module `Anthropic.Claude` re-exports all public API
- [ ] Manual test: Run tool use example, batch workflow with real API

**Time estimate**: 6 days

---

## Phase 7: Polish + Release (Week 7-8) - Documentation + CI

**Goal**: Production-ready package for Hackage release

### Deliverables

1. **Documentation**
   - Haddock for all public modules (100% coverage)
   - `README.md` with quickstart, installation, examples
   - `CHANGELOG.md`
   - `examples/` directory with runnable programs

2. **CI/CD**
   - GitHub Actions: build on GHC 9.6, 9.8, 9.10
   - Automated tests (hspec + QuickCheck)
   - `hlint` and formatting checks
   - Hackage upload workflow

3. **Package metadata**
   - `claude-sdk.cabal` with proper dependencies, bounds, metadata
   - LICENSE file
   - Stability: experimental (v0.1.0.0)

### Validation
- [ ] Haddock builds without warnings
- [ ] README examples run successfully
- [ ] CI builds pass on all GHC versions
- [ ] Package uploads to Hackage successfully

**Time estimate**: 5 days

---

## Testing Strategy

### Standard Haskell Test Organization

**Pattern**: Tests mirror source modules with `*Spec.hs` suffix
```
src/Anthropic/Claude/Types/Core.hs  →  test/Anthropic/Claude/Types/CoreSpec.hs
src/Anthropic/Claude/Internal/HTTP.hs  →  test/Anthropic/Claude/Internal/HTTPSpec.hs
```

**Auto-discovery**: `test/Spec.hs` with hspec-discover:
```haskell
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```

**Each `*Spec.hs` contains**:
- QuickCheck `Arbitrary` generators for types in that module
- Property tests (JSON roundtrip, invariants)
- Unit tests (example-based, helper functions)
- Integration tests (mock HTTP where applicable)

### Property-Based Tests (QuickCheck)

Embedded in `*Spec.hs` files using `it "property description" $ property $ \x -> ...`:

- **JSON Codecs** (`Types/*Spec.hs`):
  - Roundtrip: `decode . encode == id`
  - Discriminator correctness: `type` field matches variant
  - Field naming: snake_case in JSON, camelCase in Haskell

- **Retry Logic** (`Internal/RetrySpec.hs`):
  - Max attempts respected (with `IORef` counter)
  - Backoff calculation formula correct
  - Retryable vs non-retryable error classification

- **SSE Parsing** (`Internal/SSESpec.hs`):
  - Arbitrary bytes don't crash parser
  - Multi-line data concatenation correct

- **Streaming** (`Internal/StreamingSpec.hs`):
  - Partial recovery on error (accumulate events before failure)

### Integration Tests (hspec)

Also embedded in `*Spec.hs` files using `it "description" $ do ...`:

- **HTTP Client** (`Internal/HTTPSpec.hs`, `MessagesSpec.hs`):
  - Mock HTTP dispatch for deterministic tests
  - Request validation (headers, body serialization)
  - Response parsing (success 200, errors 400/401/429/500)
  - Rate limit header extraction

- **Streaming** (`StreamingSpec.hs`, `Internal/StreamingSpec.hs`):
  - Event sequences correct
  - Resource cleanup (bracket safety via CPS)
  - Error propagation in streams

- **End-to-End** (`ToolsSpec.hs`, `BatchSpec.hs`):
  - Tool use loop: request → extract → execute → results → final response
  - Batch workflow: create → poll → retrieve → verify

### Manual Testing (with real API key)

Run examples with `ANTHROPIC_API_KEY` set:
- Live API calls (simple message, streaming, tool use)
- Error scenarios (invalid key, rate limits)
- Long-running streams (resource cleanup verification)

### Example Test File Structure

**`test/Anthropic/Claude/Types/CommonSpec.hs`** (typical structure):

```haskell
module Anthropic.Claude.Types.CommonSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Anthropic.Claude.Types.Common
import Data.Aeson (encode, decode)

-- QuickCheck generators
instance Arbitrary ContentBlock where
  arbitrary = oneof
    [ TextBlock <$> arbitrary
    , ImageBlock <$> arbitrary
    , ToolUseBlock <$> arbitrary <*> arbitrary <*> arbitrary
    , ToolResultBlock <$> arbitrary <*> arbitrary <*> arbitrary
    ]

instance Arbitrary ImageSource where
  arbitrary = oneof
    [ Base64Source <$> arbitrary <*> arbitrary
    , URLSource <$> arbitrary
    ]

-- Test suite
spec :: Spec
spec = describe "Types.Common" $ do

  describe "ContentBlock" $ do
    -- Unit test (example-based)
    it "parses TextBlock from JSON" $ do
      let json = "{\"type\":\"text\",\"text\":\"hello\"}"
      decode json `shouldBe` Just (TextBlock "hello")

    -- Property test (JSON roundtrip)
    it "round-trips through JSON" $ property $
      \(cb :: ContentBlock) -> decode (encode cb) === Just cb

    -- Property test (discriminator)
    it "uses correct type discriminator" $ property $
      \cb -> case cb of
        TextBlock _ -> getTypeField cb === "text"
        ImageBlock _ -> getTypeField cb === "image"
        -- etc.

  describe "ImageSource" $ do
    it "parses Base64Source" $ do
      -- Unit test...

    it "round-trips through JSON" $ property $
      -- Property test...
```

**Key points**:
- `Arbitrary` instances for generators at top of file
- Mix unit tests (`it "description" $ do ...`) and property tests (`it "description" $ property $ ...`)
- Group related tests with `describe` blocks
- Export `spec :: Spec` for hspec-discover

---

## Critical Files (Highest Priority)

These 5 files are the most critical and should be implemented carefully:

1. **`src/Anthropic/Claude/Types/Common.hs`** - Discriminated unions pattern (ContentBlock, ImageSource); sets JSON pattern for all other discriminated unions
2. **`src/Anthropic/Claude/Internal/JSON.hs`** - Shared JSON infrastructure (`aesonOptions`, `parseDiscriminated`); all modules depend on these utilities
3. **`src/Anthropic/Claude/Internal/HTTP.hs`** - Core HTTP client (`ClientEnv`, `mkClientEnv`, `buildRequest`); foundation for all API operations
4. **`src/Anthropic/Claude/Internal/SSE.hs`** - SSE parser (`parseSSE`, `parseSSELine`); most complex parsing logic in the SDK
5. **`test/Anthropic/Claude/Types/CommonSpec.hs`** - Testing patterns (QuickCheck generators, property test structure); establishes conventions for all other `*Spec.hs` files

---

## Timeline Summary

**🎯 Project becomes buildable and testable: Phase 1 (1-2 hours)**

After Phase 1, `cabal build` and `cabal test` work for all remaining phases.

| Phase | Duration | Milestone |
|-------|----------|-----------|
| **1. Project Skeleton** | **1-2 hours** | **🔨 Buildable/testable foundation**<br>`cabal build` and `cabal test` work |
| 2. Foundation | 5 days | JSON codecs validated (~1,100 test cases pass) |
| 3. HTTP Infrastructure | 5 days | `createMessage` works with mock + real API |
| 4. Resilience | 4 days | Retry logic property tests pass |
| 5. Streaming | 8 days | SSE streaming works, partial recovery validated |
| 6. Batch + Tools | 6 days | Full API coverage, end-to-end tests pass |
| 7. Polish + Release | 5 days | Hackage upload succeeds |
| **Total** | **~34 days** | **6-7 weeks** |

**Buffer**: +1 week for unknowns = **7-8 weeks total**

---

## Verification (How to Test End-to-End)

After implementation, verify the SDK works correctly:

### 1. Run Full Test Suite
```bash
cabal test
# Runs all *Spec.hs files via hspec-discover
# Should pass ~3,000+ test cases:
#   - Property tests: 100 examples × 30 types = ~3,000 QuickCheck cases
#   - Unit tests: ~200 example-based tests
#   - Integration tests: ~50 mock HTTP tests
```

### 2. Run Specific Module Tests
```bash
# Test just JSON codecs
cabal test --test-option="--match=/Types/"

# Test just HTTP infrastructure
cabal test --test-option="--match=/Internal.HTTP/"

# Test just streaming
cabal test --test-option="--match=/Streaming/"
```

### 3. Manual Test (Real API)
```bash
export ANTHROPIC_API_KEY="sk-ant-..."
cabal run example-simple-message
cabal run example-streaming
cabal run example-tool-use
```

Expected output:
- Simple message: Response with text content
- Streaming: Events printed incrementally, final message with usage stats
- Tool use: Loop completes (request → tool calls → results → final answer)

### 4. Error Scenarios
```bash
export ANTHROPIC_API_KEY="invalid"
cabal run example-simple-message
# Should fail with AuthenticationError

# Rate limit test (send 100 requests quickly)
cabal run example-rate-limit
# Should see retry backoff, eventual success or rate limit error
```

### 5. CI Validation
```bash
# Push to GitHub, verify Actions pass:
- GHC 9.6, 9.8, 9.10 build
- All tests pass
- hlint warnings = 0
```

---

## Next Steps to Begin Implementation

**Start with Phase 1: Project Skeleton** (1-2 hours)

1. **Run Phase 1** (see full details in Phase 1 section above):
   ```bash
   cd /Users/gnoel5/Projects/probe/claude-sdk
   cabal init --lib --package-name=claude-sdk --license=MIT
   # Configure claude-sdk.cabal (library + test suite)
   # Create directory structure
   # Create test/Spec.hs
   # Verify: cabal build && cabal test
   ```

2. **After Phase 1 completes, development workflow**:

   **For each module**:
   - Implement `src/Anthropic/Claude/ModuleName.hs`
   - Write `test/Anthropic/Claude/ModuleNameSpec.hs` with properties + unit tests
   - Run `cabal test` - all tests should pass before proceeding

   **Development iteration**:
   ```bash
   # After implementing a module
   cabal build                              # Verify it compiles
   cabal test                               # Run all tests
   cabal test --test-show-details=direct   # See detailed output
   ```

3. **Checkpoint at end of each phase**:
   - Review validation checklist
   - Verify alignment with ADRs (see `/Users/gnoel5/Projects/design/anthropic/adr/`)
   - Update plan if needed

**First implementation work**: Phase 2 - implement `Types.Core` + `Types/CoreSpec.hs`
