# OpenCode Notification Plugin

Push notifications for OpenCode events via ntfy.sh.

## Features

Sends ntfy.sh push notifications to your phone:

- **Session Idle**: When OpenCode finishes work and is ready for your next request
- **Session Error**: When an error occurs during a session

Each notification includes the project name from the current directory.

## Requirements

- `$NTFY_TOPIC` environment variable set with your ntfy.sh topic
- ntfy.sh service accessible

## Installation

1. Set your ntfy topic in `~/.bashrc` or `~/.zshrc`:
   ```bash
   export NTFY_TOPIC=your-topic-name
   ```

2. The plugin is automatically loaded from `~/.config/opencode/plugin/notify.js`.

## Notification Details

| Event | Priority | Tags | Message |
|-------|----------|------|---------|
| Session Idle | default | white_check_mark | "Session completed! Ready for your next request." |
| Session Error | urgent | fire | "Session error occurred. Check the terminal for details." |

All notifications include the Click header `android-app://com.termux` to open Termux when tapped.

## Customization

Edit `~/.config/opencode/plugin/notify.js` to customize:
- Notification messages
- Priority levels (`min`, `low`, `default`, `high`, `urgent`)
- Tags (`white_check_mark`, `warning`, `fire`, `rocket`, `bell`, `computer`, `tada`)
- Which events trigger notifications
