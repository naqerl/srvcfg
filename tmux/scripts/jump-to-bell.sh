#!/bin/sh
# Jump to the most recent belled window
# Usage: Bind to a key in tmux, e.g., bind-key b run-shell "~/.tmux/scripts/jump-to-bell.sh"

BELL_FILE="$HOME/.tmux/bells/bells.jsonl"

if [ ! -f "$BELL_FILE" ] || [ ! -s "$BELL_FILE" ]; then
    tmux display-message "No bells pending"
    exit 0
fi

# Get the most recent bell (last line of JSONL)
LATEST_BELL=$(tail -n 1 "$BELL_FILE")

if [ -z "$LATEST_BELL" ]; then
    tmux display-message "No bells pending"
    exit 0
fi

# Extract session and window
SESSION_NAME=$(echo "$LATEST_BELL" | jq -r '.session')
WINDOW_INDEX=$(echo "$LATEST_BELL" | jq -r '.window')

if [ -z "$SESSION_NAME" ] || [ "$SESSION_NAME" = "null" ]; then
    tmux display-message "Invalid bell data"
    exit 0
fi

# Check if session still exists (skip stale bells)
if ! tmux has-session -t "$SESSION_NAME" 2>/dev/null; then
    # Session is gone, remove this stale bell and try next
    # For now just show message - a full implementation would iterate through bells
    tmux display-message "Session $SESSION_NAME no longer exists (stale bell)"
    exit 0
fi

# Switch to the target session and window
tmux switch-client -t "$SESSION_NAME:$WINDOW_INDEX" 2>/dev/null || \
    tmux attach-session -t "$SESSION_NAME:$WINDOW_INDEX" 2>/dev/null || \
    tmux display-message "Cannot switch to $SESSION_NAME:$WINDOW_INDEX"

exit 0
