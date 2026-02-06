#!/bin/sh
# Log a bell event to the attention bus
# Usage: Called by tmux alert-bell hook
# Skips logging if bell comes from currently focused window/pane

BELL_FILE="$HOME/.tmux/bells/bells.jsonl"
mkdir -p "$HOME/.tmux/bells"

# Get session info from tmux hook arguments
SESSION_NAME="$1"
WINDOW_INDEX="$2"
WINDOW_NAME="$3"

# Get current (focused) session and window from tmux
CURRENT_SESSION=$(tmux display-message -p '#{session_name}' 2>/dev/null)
CURRENT_WINDOW=$(tmux display-message -p '#{window_index}' 2>/dev/null)

# Skip if bell came from currently focused window
if [ "$SESSION_NAME" = "$CURRENT_SESSION" ] && [ "$WINDOW_INDEX" = "$CURRENT_WINDOW" ]; then
    # Bell came from the window we're currently looking at - don't log
    exit 0
fi

TIMESTAMP=$(date -Iseconds)

# Append to JSONL file
printf '{"session":"%s","window":%s,"name":"%s","time":"%s"}\n' \
    "$SESSION_NAME" "$WINDOW_INDEX" "$WINDOW_NAME" "$TIMESTAMP" >> "$BELL_FILE"

# Refresh status bar to show the bell immediately
tmux refresh-client -S

exit 0
