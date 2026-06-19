# JSON in the center panel — two types

## 1. Tool definition (function calling)

Detected when JSON contains `"type": "function"`.

The AI can **call a function** — it returns a structured tool_call instead of plain text.

```json
{
  "type": "function",
  "function": {
    "name": "my_tool",
    "description": "What the tool does",
    "parameters": {
      "type": "object",
      "properties": {
        "param1": { "type": "string", "description": "..." }
      },
      "required": ["param1"]
    }
  }
}
```

**OpenAI / Mistral** — sent as-is in `"tools": [...]`

**Anthropic** — auto-converted: wrapper removed, `parameters` renamed to `input_schema`

---

## 2. Response format schema (structured output)

Detected when JSON does **not** contain `"type": "function"`.

The AI returns plain text but **forced into a specific JSON shape**.

```json
{
  "type": "object",
  "properties": {
    "answer": { "type": "string" },
    "confidence": { "type": "number" }
  },
  "required": ["answer", "confidence"]
}
```

**OpenAI / Mistral** — wrapped in `"response_format": { "type": "json_schema", ... }`

**Anthropic** — wrapped in `"output_config": { "format": { "type": "json_schema", ... } }`

---

## Summary

| | Tool definition | Response schema |
|---|---|---|
| Marker | `"type": "function"` present | absent |
| AI returns | tool_call (name + arguments) | plain text in JSON shape |
| Use case | agent / agentic loop | extract structured data |
