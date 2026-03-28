# Missing API Features

API capabilities not yet modeled in the SDK.

## 1. Computer Use / Bash Tool Types

Beyond `type: "custom"`, the API supports built-in tool types: `type: "computer_20241022"` (screen control), `type: "text_editor_20241022"`, and `type: "bash_20241022"`. Each has a distinct schema. The current `Tool` type hardcodes `type: "custom"` and rejects others in `FromJSON`.

