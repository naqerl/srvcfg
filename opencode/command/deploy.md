---
description: Set up deployment infrastructure for the project
agent: deploy
subtask: false
---

Analyze the current project and set up comprehensive deployment infrastructure following aishift patterns.

Your task:
1. Detect the project type (Python, Golang, React, or combination)
2. Fetch deployment templates from vps-setup repository (versioned)
3. Ask necessary configuration questions (target server, environments, etc.)
4. Generate deploy/ directory with all required files
5. Create systemd services, sudoers configs, and Makefile targets
6. Add Caddy configuration if reverse proxy is needed
7. Update project documentation with deployment instructions

If arguments are provided ($ARGUMENTS), use them as answers to configuration questions or as additional context.

Examples:
- `/deploy` - Interactive setup with questions
- `/deploy production vps.example.com` - Set up for production environment on specified server
- `/deploy staging production` - Set up both staging and production environments
- `/deploy upgrade` - Upgrade existing deployment to newer vps-setup version

Always provide clear next steps after generating deployment files.
