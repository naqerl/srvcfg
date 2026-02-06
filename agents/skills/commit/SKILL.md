---
name: commit
description: Create git commits for staged or unstaged changes. Use when the user explicitly requests to commit changes with phrases like "commit", "commit changes", "make a commit", "create commit", or when the user directly asks to save changes to git. Do NOT use this skill unless the user has explicitly requested a commit action - never commit changes automatically or without direct user instruction.
---

# Git Commit Skill

Create well-organized git commits following best practices.

## When to Use

Only use this skill when:
- The user explicitly says "commit", "commit changes", "make a commit"
- The user directly requests to save changes to git
- The user asks to group and commit changes

**IMPORTANT**: Never initiate committing without direct user request. Do not commit as a side effect of other tasks.

## Workflow

1. **Check git status** - See what changes are present (staged/unstaged)
2. **Analyze changes** - Review diffs to understand what changed
3. **Group logically** - Organize changes into logically connected groups
4. **Stage and commit** - Add files and create commits with descriptive messages

## Commit Message Guidelines

- Use semantic prefixes based on feature/area (e.g., `docs:`, `project:`, `customers:`)
- Group related changes into single commits
- Avoid generic prefixes like `feat:`, `chore:`, `fix:`, `refactor:`, `add:`
- Examples:
  - `docs: actualized spec for feature A`
  - `customers: UI shows actual balance`
  - `project: make target for CD`

## Steps

1. Run `git status` to see current state
2. Run `git diff` to understand changes
3. Group changes by logical connection/area
4. Stage files for each group (`git add <files>`)
5. Create commit with descriptive message (`git commit -m "<message>"`)
6. Repeat until all changes are committed
