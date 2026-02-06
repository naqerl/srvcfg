---
name: plan
description: Build structured implementation plans with problem analysis, solution design, and nested task breakdowns. Creates tasks/todo.md files with problem statement, solution approach, and actionable nested todos. Handles existing todo.md files by prompting for truncation or archival.
type: flow
---

# Plan Building Flow

This flow guides through creating a structured plan document at `tasks/todo.md`.

```mermaid
flowchart TD
    A([BEGIN]) --> B{Check tasks/todo.md exists?}
    B -->|Yes| C[Read existing todo.md and summarize plan + progress]
    C --> D[Prompt user: 1=truncate, 0=rename & create new]
    D --> E{User choice?}
    E -->|1| F[Proceed with creating new plan]
    E -->|0| G[Rename todo.md with kebab-case archive name]
    G --> F
    B -->|No| F
    F --> H[Gather context: Check AGENTS.md, README, and project structure]
    H --> I[Define problem: Write clear problem statement under # Problem]
    I --> J{Is the problem well-defined?}
    J -->|No| K[Ask user clarifying questions about scope, constraints]
    K --> I
    J -->|Yes| L[Design solution: Write approach under # Solution]
    L --> M{Is the solution clear?}
    M -->|No| N[Research alternatives or ask about preferences]
    N --> L
    M -->|Yes| O[Break down tasks: Create nested todo list under # Tasks]
    O --> P{Are tasks small and actionable?}
    P -->|No| Q[Decompose large tasks with 2-space indentation]
    Q --> O
    P -->|Yes| R[Review: Read todo.md and verify structure]
    R --> S[Archive: Rename todo.md with kebab-case name describing plan]
    S --> T([END])
```

## Handling Existing todo.md

Before creating a new plan:

1. **Check for existing file**: Look for `tasks/todo.md`
2. **If exists**: Read and summarize:
   - The problem statement (brief)
   - Overall progress (% complete or tasks done/total)
3. **Prompt user**:
   ```
   Existing plan found: [brief description]
   Progress: [X% complete or X/Y tasks done]
   
   Choose action:
   1 - Truncate (overwrite with new plan)
   0 - Archive current and create new
   ```
4. **If user chooses 0** (rename): Generate kebab-case archive name (max 5 words) describing the plan, e.g., `refactor-auth-module.md`, `implement-user-dashboard.md`, `fix-memory-leak-issue.md`

## Output Format

The flow creates `tasks/todo.md` with this structure:

```markdown
# Problem

Clear statement of what needs to be solved.

# Solution

High-level approach to solving the problem.

# Tasks

## [Category/Phase]
- [ ] Task 1
  - [ ] Subtask 1.1
  - [ ] Subtask 1.2
- [ ] Task 2
```

## Task Nesting Rules

- Use `- [ ]` for all task items
- Indent subtasks with 2 spaces: `  - [ ] Subtask`
- Group under phase headers: `## Phase Name`
- Keep nesting to 2-3 levels maximum

## Archiving Completed Plans

At the end of the flow, archive the todo.md:

1. Generate a concise kebab-case name (max 5 words) based on the problem/solution
2. Rename `tasks/todo.md` to `tasks/{kebab-name}.md`
3. Examples:
   - Plan: "Implement user authentication with JWT tokens" → `implement-user-auth-jwt.md`
   - Plan: "Fix critical memory leak in data processing pipeline" → `fix-memory-leak-pipeline.md`
   - Plan: "Refactor database connection handling for better performance" → `refactor-db-connection-performance.md`
