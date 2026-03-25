# Missing API Features

API capabilities not yet modeled in the SDK.

## 1. Extended Thinking

Lets Claude show its chain-of-thought reasoning before answering. Requires a `thinking` field on the request (`{ type: "enabled", budget_tokens: N }`) and a new `ThinkingBlock` content block type in responses/streaming. Also introduces a `thinking_delta` `ContentDelta` variant for streaming thinking text. The `budget_tokens` replaces `max_tokens` as the primary length control when thinking is enabled.

## 2. Prompt Caching (per-block / system-level)

The SDK has `CacheControl` on `Tool`, but the API also supports `cache_control` on individual content blocks within messages, on system prompt blocks, and on tool results. The `requestSystem` field is currently `Maybe Text` but the API accepts a list of system content blocks (text with optional `cache_control`), enabling partial caching of multi-part system prompts.

## 3. PDF / Document Support

A `document` content block type (`type: "document"`) that accepts base64-encoded PDFs (and potentially other document types). Structurally similar to `ImageSource` — has `source` with `type: "base64"`, `media_type: "application/pdf"`, and `data`.

## 4. Token Counting Endpoint

`POST /v1/messages/count_tokens` — takes the same request shape as `createMessage` and returns `{ input_tokens: N }` without actually running inference. Useful for cost estimation and prompt length validation before sending.

## 5. Citations

When enabled via a request parameter (`citations: { enabled: true }`), Claude can return citation markers in text that reference specific passages from the input. Introduces `CitationBlock` content in responses and associated source location types.

## 6. Computer Use / Bash Tool Types

Beyond `type: "custom"`, the API supports built-in tool types: `type: "computer_20241022"` (screen control), `type: "text_editor_20241022"`, and `type: "bash_20241022"`. Each has a distinct schema. The current `Tool` type hardcodes `type: "custom"` and rejects others in `FromJSON`.

## 7. Usage Detail Fields

The current `Usage` type only has `inputTokens` and `outputTokens`. The API also returns `cache_creation_input_tokens`, `cache_read_input_tokens` (for prompt caching), and when extended thinking is enabled, separate thinking token counts.

## 8. Streaming: Thinking Deltas

The `ContentDelta` type handles `text_delta` and `input_json_delta`, but extended thinking adds `thinking_delta` (streaming thinking text) and `signature_delta` events. These would need new constructors.

## 9. Multi-modal Tool Results

`ToolResultBlock` currently has `blockToolResult :: Value`, but the API accepts a list of content blocks (text + images) as tool results, not just raw JSON. This matters for computer-use workflows where screenshots are returned as tool results.

## 10. Service Tier

The API supports a `service_tier` field on requests (e.g., `"standard"`, `"priority"`) and returns it in the response. Not currently modeled.
