# Service Configuration Repository (srvcfg)

## Project Overview

This is a **personal dotfiles and service configuration repository** maintained by `scipunch@gmail.com`. It contains development environment configurations, shell customizations, editor settings, and AI agent definitions for a Linux-based development workflow.

The repository serves as a single source of truth for:
- Shell environment configuration (Bash)
- Editor configuration (Emacs)
- Terminal multiplexer settings (Tmux)
- AI agent configurations (OpenCode, Kimi CLI)
- Custom utility scripts

## Repository Structure

```
srvcfg/
├── bin/                    # Custom shell scripts
│   └── pj                  # Project jumper with tmux session management
├── emacs/                  # Emacs configuration
│   ├── init.el            # Main Emacs configuration
│   └── make-completion.el # Makefile target completion package
├── tmux/                   # Tmux configuration
│   └── tmux.conf          # Tmux settings and keybindings
├── opencode/               # OpenCode AI agent configuration
│   ├── config.json        # Main OpenCode config with MCP servers
│   ├── opencode.json      # Additional MCP configuration
│   ├── package.json       # Node.js dependencies for plugins
│   ├── agent/             # Agent definitions (markdown with YAML frontmatter)
│   ├── command/           # Custom slash commands
│   └── plugin/            # Custom JavaScript plugins
├── .agents/                # Kimi CLI agent configurations
│   └── plan-agent/        # Plan-and-execute agent implementation
├── tasks/                  # Task tracking (excluded from git)
│   └── todo.md
├── .bashrc                 # Bash shell configuration
├── install.sh              # Installation script for symlinking configs
└── .gitignore              # Git ignore rules
```

## Technology Stack

| Component | Technology | Purpose |
|-----------|------------|---------|
| Shell | Bash | Primary shell environment |
| Editor | GNU Emacs 29+ | Primary code editor |
| Terminal Multiplexer | Tmux | Session management |
| AI Agents | OpenCode, Kimi CLI | AI-powered development assistance |
| Package Manager | npm/bun | Node.js dependencies for OpenCode |
| Language | JavaScript/Node.js | OpenCode plugins |
| Language | Emacs Lisp | Editor customization |
| Language | Python 3 | Utility scripts |

## Installation and Setup

### Initial Installation

Run the install script to create symbolic links:

```bash
./install.sh
```

This script creates symlinks from this repository to the appropriate locations in `$HOME`:
- `emacs/*` → `~/.emacs.d/`
- `tmux/tmux.conf` → `~/`
- `.bashrc` → `~/`
- `opencode/` → `~/.config/`
- `bin/*` → `~/.local/bin/`

### Environment Variables

The following environment variables are configured in `.bashrc`:

| Variable | Description |
|----------|-------------|
| `GOPATH` | Go workspace path (`$HOME/go`) |
| `PNPM_HOME` | pnpm package manager home |
| `PATH` | Extended with custom bins and tools |
| `OPENCODE_EXPERIMENTAL_PLAN_MODE` | Enable OpenCode plan mode |
| `OPENCODE_ENABLE_EXA` | Disable Exa search |
| `EDITOR` | Set to `emacs` |
| `NTFY_TOPIC` | Required for push notifications (set in `~/.env`) |

### Required External Dependencies

- `fzf` - Fuzzy finder for the `pj` script
- `tmux` - Terminal multiplexer
- `emacs` - GNU Emacs 29 or later
- `bun` or `npm` - For OpenCode plugin dependencies
- `curl` - For ntfy.sh notifications

## OpenCode Configuration

### MCP Servers

Configured in `opencode/config.json` and `opencode/opencode.json`:

| Server | Type | Status | Description |
|--------|------|--------|-------------|
| tavily | Remote | Enabled | Web search and research |
| miro | Remote | Enabled | Whiteboard integration |
| asana | Remote | Disabled | Task management |
| telegram | Local | Disabled | Messaging integration |

### Custom Agents

Located in `opencode/agent/`, defined as Markdown with YAML frontmatter:

| Agent | Mode | Model | Purpose |
|-------|------|-------|---------|
| `git` | subagent | opencode/grok-code | Git commit creation and management |
| `jedi` | primary | opencode/glm-4.7 | Personal task management using Attention Management principles |
| `knowledge` | subagent | default | Fetches Go development patterns from private repo |
| `watson` | primary | opencode/grok-code | Production issue investigation |

### Custom Commands

Located in `opencode/command/`:

| Command | Agent | Description |
|---------|-------|-------------|
| `/deploy` | deploy | Set up deployment infrastructure |
| `/commit` | git | Commit current changes |
| `/memory` | general | Add new instruction to AGENTS.md |

### Plugins

**notify.js** - Sends push notifications via ntfy.sh:
- Session idle notifications (low priority)
- Session error notifications (urgent priority)

Requires `NTFY_TOPIC` environment variable to be set.

## Kimi CLI Plan Agent

Located in `.agents/plan-agent/`, implements a Plan-and-Execute workflow:

### Structure

```
plan-agent/
├── plan-agent.yaml           # Main agent configuration
├── executor-subagent.yaml    # Executor configuration
├── prompts/
│   ├── planner-prompt.md    # Planning mode system prompt
│   └── executor-prompt.md   # Execution mode system prompt
├── references/
│   ├── planning-patterns.md # Common planning patterns
│   └── examples.md          # Real-world plan examples
└── scripts/
    └── plan-validator.py    # Plan structure validator
```

### Workflow

1. **Plan Mode**: Read-only analysis, context gathering, plan creation
2. **User Review**: Plan presented for approval/modification
3. **Execute Mode**: Step-by-step plan execution with progress tracking
4. **Replanning**: Dynamic replanning when blockers occur

### Tools Available

- Shell execution
- File operations (read, write, glob, grep)
- Web search and fetch
- Subagent spawning (planner only)
- Todo list management

## Development Conventions

### Agent Definition Format

Agents are defined in Markdown files with YAML frontmatter:

```yaml
---
description: Brief description of agent purpose
mode: primary|subagent
model: model-name (optional)
tools:
  write: true|false
  edit: true|false
  bash: true|false
---

# Agent instructions in Markdown...
```

### AI Agent Instructions

When working in projects configured by this repository:

1. **Makefile Usage**: Projects include Makefiles for build/test/deploy tasks. Always look for and use existing Makefiles before creating new ones.

2. **Environment Variables**: Makefiles automatically source `.env` files. Do not read `.env` files directly - let Make handle them.

3. **Web Search**: Use Tavily MCP for fresh documentation, best practices, and existing solutions.

4. **Question Handling**: When users ask "why", "what", "how" questions, provide explanations without making modifications unless explicitly requested.

### Git Conventions

Based on `opencode/agent/git.md`:

- Use semantic commit prefixes based on feature/area (e.g., `docs:`, `project:`, `customers:`)
- Group related changes into single commits
- Avoid prefixes like `feat:`, `chore:`, `fix:`, `refactor:`, `add:`
- Examples:
  - `docs: actualized spec for feature A`
  - `customers: UI shows actual balance`
  - `project: make target for CD`

## Tmux Workflow

The `pj` (project jump) script creates a standardized tmux session structure:

```
Session: [project-name]
├── Window 1: emacs (editor)
├── Window 2: shell (default working directory)
└── Window 3: agent (kimi AI assistant)
```

### Key Bindings

| Key | Action |
|-----|--------|
| `C-z` | Prefix key |
| `C-z c` | New window (current path) |
| `C-z p` | Open project jumper (`pj` command) |
| `C-z r` | Reload config |
| `C-z t` | Send BackTab (for cycling agents on mobile) |

## Emacs Configuration

### Key Bindings

| Key | Action |
|-----|--------|
| `F8` | Recompile (project or current) |
| `F9` | Interactive make target selection |
| `F10` | Compile (project or current) |
| `C-x ;` | Comment line |
| `C-x /` | Comment/uncomment region |
| `C-x g` | Magit status |
| `C-c d` | Duplicate line |
| `C-c r` | Replace regexp |
| `C-c R` | Replace string |
| `M-;` | Expand region |
| `M-i` | Change inner |
| `M-o` | Change outer |
| `C-w` | Smart kill (region or backward word) |
| `C-M-p` | Previous buffer |
| `C-M-n` | Next buffer |

### Packages Used

- `magit` - Git interface
- `orderless` - Completion style
- `expand-region` - Region expansion
- `embark` - Contextual actions
- `change-inner` - Text objects
- `dumb-jump` - Jump to definition
- `golden-ratio` - Window management
- `go-mode` - Go language support
- `markdown-mode` - Markdown support

## Security Considerations

1. **Sensitive Files**: The following files contain sensitive data and are managed outside git or encrypted:
   - `opencode/antigravity-accounts.json` - Contains refresh tokens
   - `~/.env` - Environment secrets (loaded by `.bashrc`)

2. **MCP Server Permissions**: 
   - Telegram MCP is disabled by default
   - Asana MCP is disabled by default
   - Remote MCP servers (Tavily, Miro) are enabled

3. **Claude Permissions**: Configured in `.claude/settings.local.json` to allow specific system commands.

## Testing

No automated test suite exists for this repository. Changes should be validated by:

1. Running `install.sh` to verify symlinks create correctly
2. Sourcing `.bashrc` to check for shell errors
3. Starting Emacs to verify init.el loads without errors
4. Starting Tmux to verify config loads
5. Testing the `pj` script with a git repository

## Backup and Migration

To migrate to a new system:

1. Clone this repository
2. Install external dependencies (Emacs, Tmux, fzf, etc.)
3. Run `./install.sh`
4. Create `~/.env` with required secrets:
   ```bash
   export NTFY_TOPIC=your-topic-name
   # Other API keys and secrets
   ```
5. Restart shell or source `~/.bashrc`

## License

Components in this repository are licensed under their respective licenses:
- `make-completion.el` - GPL v3 or later
- OpenCode plugins - Follow project conventions
