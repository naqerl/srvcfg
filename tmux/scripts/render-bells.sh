#!/bin/sh
# Render tmux bells for status line
# Usage: Add to tmux status-right: #(~/.tmux/scripts/render-bells.sh)

BELL_FILE="$HOME/.tmux/bells/bells.jsonl"

if [ ! -f "$BELL_FILE" ] || [ ! -s "$BELL_FILE" ]; then
    exit 0
fi

# Read bells and filter out those from non-existent sessions
# Output format: 🔥session(count) 🔥session2(count)
jq -r '.session' "$BELL_FILE" 2>/dev/null | \
    sort | \
    uniq -c | \
    while read -r count session; do
        # Only show if session still exists
        if tmux has-session -t "$session" 2>/dev/null; then
            printf "🔔%s(%s) " "$session" "$count"
        fi
    done

exit 0
