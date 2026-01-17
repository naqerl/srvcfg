# Project linting and building
Do not run any linting tools directly line `npm eslit` or `go build`.
Always use project's `Makefile`, it's the only proper way to check validity of the code.
Makefiles may be nested in the corresponding directory in the project, so if you do not see needed target in the root one, try searching for other Makefiles

# Git commits
When the user asks to commit changes, always use the Task tool with `subagent_type="git"` to launch the specialized git agent. Do not handle git commits directly.

The git agent should only commit changes when explicitly asked by the user. Do not automatically commit other changes or be proactive about committing - wait for explicit user instruction to commit.

# Deployment infrastructure
When the user asks to set up deployment, create deployment files, or configure CD/CI for Python, Golang, or React projects, use the Task tool with `subagent_type="deploy"` to launch the specialized deployment agent. Do not handle deployment setup directly.

Common use cases:
- Setting up initial deployment infrastructure (deploy/ directory, systemd services, Makefiles)
- Creating deployment configuration for new projects
- Upgrading existing deployment to newer vps-setup versions
- Adding staging/production environments
- Configuring Caddy reverse proxy for web services
- Migrating deployment patterns between projects

Example: When asked "set up deployment for this project", use the deploy agent to:
1. Analyze project to detect language and build process
2. Fetch templates from vps-setup repository (with version tracking)
3. Generate deploy/ directory with all necessary files
4. Provide clear documentation and next steps

# Go development patterns
When planning new features, adding handlers, services, or any significant code changes in Go projects, use the Task tool with `subagent_type="knowledge"` to fetch relevant patterns and best practices from the knowledge base.

Common use cases:
- Adding new HTTP handlers or routes
- Implementing services (interface-based or channel-based)
- Working with SQLC queries and transactions
- Adding HTMX interactivity
- Telegram bot integration

Example: When asked "add a new endpoint for user profiles", use the knowledge agent to:
1. Fetch relevant patterns for handlers and routing
2. Apply the patterns to implement the feature

# Tmux sessions and process monitoring
When the user asks to check logs, monitor running processes, or run interactive/long-running commands, use the Task tool with `subagent_type="tmux"` to launch the specialized tmux agent. Do not handle tmux operations directly.

Common use cases:
- Checking logs for errors (especially in window index 1 where application logs typically run)
- Running long-running build/test commands that shouldn't block the main session
- Starting interactive commands that require user input
- Monitoring background processes

Example: When asked "are there any errors in the logs?", use the tmux agent to:
1. List available sessions
2. Capture pane content from window index 1 (common location for tail/log watching)
3. Search for error patterns in the captured output

# Behaviour
- Do not create summary documents until directly asked to do it
- Do not write a summary of what you've done until directly asked to do it
- Ensure that summary of changes will not be created after finishing work

# Responding to questions
When the user asks questions like "why", "what", "how", or any other informational question, treat it as a request for explanation or information. Do not interpret these questions as indirect requests to modify code or make changes. The user wants to understand reasons, decisions, and context - simply provide clear answers and explanations without making any modifications unless explicitly requested.