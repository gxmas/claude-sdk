# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0.0] - 2026-03-21

### Added
- Initial release of the Claude SDK for Haskell
- **Messages API**: `createMessage` with full request/response types
- **Streaming API**: SSE-based streaming with `withMessageStream`, `forEachEvent`, `throwOnError`
- **Batch API**: `createBatch`, `retrieveBatch`, `listBatches`, `cancelBatch`, `pollBatchUntilDone`
- **Tool use helpers**: `defineTool`, `extractToolCalls`, `buildToolResult`, `buildToolError`
- **Retry logic**: Configurable exponential backoff for transient errors (429, 500, 529)
- **Rate limit tracking**: `APIResponse` wrapper with rate limit metadata from headers
- **Type safety**: Discriminated unions for `ContentBlock`, `StreamEvent`, `ToolChoice`, `BatchResultData`
- **Error handling**: Network errors as exceptions, API errors in `Either` (ADR 0002)
- **MonadUnliftIO**: All operations work in `IO`, `ReaderT`, and custom monad stacks
- **Model constants**: `claude4Opus`, `claude4Sonnet`, `claude35Haiku`
- **Example executable**: `single-prompt` for interactive testing
