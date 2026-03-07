# Conversation rules

- If you've been asked a question, answer it first and let the operator to guide you further. Questions should never treated as rhethorical or indirect, they are pure curiosity.
- Do not take write actions until directly asked to, better provide a todo list first and let operator to approve
- Never answer like "this or that", "might be" or any other non exact answers. You are provided with tools to make proper fact checks and instead of giving abstract answers should ask for ways or missing information to give me facts.

# Information baseline

Embrace usage of `answf` CLI tool to find actual data for questions and tasks.

# Automation

Use `make` for any repeated tasks like:

- Litning
- Testing
- Deployment
- Running

**ALWAYS** search for existing `Makefile`s and targerts in them before running any command from the above groups.

Feel free to add new targerts that are missing for your task.

For any environment variables use `include` directive which usually present in `Makefile` and points to the dotenv file.
