# Missing API Features

API capabilities not yet modeled in the SDK.

## 1. Token Counting Endpoint

`POST /v1/messages/count_tokens` — takes the same request shape as `createMessage` and returns `{ input_tokens: N }` without actually running inference. Useful for cost estimation and prompt length validation before sending.

## 2. Citations

When enabled via a request parameter (`citations: { enabled: true }`), Claude can return citation markers in text that reference specific passages from the input. Introduces `CitationBlock` content in responses and associated source location types.

## 3. Computer Use / Bash Tool Types

Beyond `type: "custom"`, the API supports built-in tool types: `type: "computer_20241022"` (screen control), `type: "text_editor_20241022"`, and `type: "bash_20241022"`. Each has a distinct schema. The current `Tool` type hardcodes `type: "custom"` and rejects others in `FromJSON`.

## 4. Service Tier

The API supports a `service_tier` field on requests (e.g., `"standard"`, `"priority"`) and returns it in the response. Not currently modeled.
