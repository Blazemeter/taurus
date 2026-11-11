# Filters `claude --print --verbose --output-format stream-json` down to
# session start, the model's own narration text, and the final result -
# deliberately omitting individual tool calls/results (Bash commands,
# poll loops, etc.). Progress visibility this way depends on the model
# narrating stage transitions on its own; it will not show anything during
# a single long wait inside one step (e.g. the unit test run or a
# branch-build/rescan poll loop).
if .type == "system" and .subtype == "init" then
  "▶️  session started (model: \(.model))"
elif .type == "assistant" then
  (.message.content[]? | select(.type == "text") | "💬 " + .text)
elif .type == "result" then
  "\n=== FINAL (\(.subtype), \(.duration_ms)ms, $\(.total_cost_usd)) ===\n" + .result
else empty
end
