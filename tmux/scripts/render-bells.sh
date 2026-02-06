#!/bin/sh
# Render tmux bells for status line
# Usage: Add to tmux status-right: #(~/.tmux/scripts/render-bells.sh)

BELL_FILE="$HOME/.tmux/bells/bells.jsonl"

if [ ! -f "$BELL_FILE" ] || [ ! -s "$BELL_FILE" ]; then
    exit 0
fi

# Output format: 🔥session(count) 🔥session2(count)
jq -r '.session' "$BELL_FILE" 2>/dev/null | \
    sort | \
    uniq -c | \
    awk '{printf "🔔%s(%s) ", $2, $1}'
