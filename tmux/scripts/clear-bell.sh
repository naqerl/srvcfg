#!/bin/sh
# Clear bell for current session/window when focused
# Usage: Called by tmux client-focus-in hook

BELL_FILE="$HOME/.tmux/bells/bells.jsonl"
TEMP_FILE="$HOME/.tmux/bells/bells.jsonl.tmp"

SESSION_NAME="$1"
WINDOW_INDEX="$2"

if [ -f "$BELL_FILE" ]; then
    # Remove matching entries
    jq -c "select(.session != \"$SESSION_NAME\" or .window != $WINDOW_INDEX)" "$BELL_FILE" > "$TEMP_FILE" 2>/dev/null
    mv "$TEMP_FILE" "$BELL_FILE"
    
    # Refresh status bar to clear the bell icon immediately
    tmux refresh-client -S
fi

exit 0
