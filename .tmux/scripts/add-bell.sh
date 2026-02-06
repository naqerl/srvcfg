#!/bin/sh
# Add a bell entry to the bells tracking file
# Used by kimi-bell wrapper

BELL_DIR="$HOME/.tmux/bells"
BELL_FILE="$BELL_DIR/bells.jsonl"

# Create directory if it doesn't exist
mkdir -p "$BELL_DIR"

# Get current session name
SESSION=$(tmux display-message -p '#S' 2>/dev/null)
if [ -z "$SESSION" ]; then
    SESSION="unknown"
fi

# Get current window name
WINDOW=$(tmux display-message -p '#W' 2>/dev/null)
if [ -z "$WINDOW" ]; then
    WINDOW="unknown"
fi

# Add bell entry with timestamp
jq -n \
    --arg session "$SESSION" \
    --arg window "$WINDOW" \
    --arg timestamp "$(date -Iseconds)" \
    '{session: $session, window: $window, timestamp: $timestamp}' >> "$BELL_FILE"

# Keep only last 100 entries to prevent file growth
tail -n 100 "$BELL_FILE" > "$BELL_FILE.tmp" && mv "$BELL_FILE.tmp" "$BELL_FILE"
