#!/bin/sh
# Log a bell event to the attention bus
# Usage: Called by tmux alert-bell hook

BELL_FILE="$HOME/.tmux/bells/bells.jsonl"
mkdir -p "$HOME/.tmux/bells"

# Get session info from tmux
SESSION_NAME="$1"
WINDOW_INDEX="$2"
WINDOW_NAME="$3"
TIMESTAMP=$(date -Iseconds)

# Append to JSONL file
printf '{"session":"%s","window":%s,"name":"%s","time":"%s"}\n' \
    "$SESSION_NAME" "$WINDOW_INDEX" "$WINDOW_NAME" "$TIMESTAMP" >> "$BELL_FILE"

# Refresh status bar to show the bell immediately
tmux refresh-client -S

exit 0
