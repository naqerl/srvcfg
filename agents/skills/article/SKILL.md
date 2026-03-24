---
name: article
description: algorithm of capturing an article for the given insight
---

Articles live in my own wiki and it's path is set to `WIKI_PATH` env variable.

If it does not set, tell about it to the user and halt until future instructions

For the wiki structure and article guidelines refer to `$WIKI_PATH/AGENTS.md`.

Produced articles are made for the public read. Details like

- Project name
- Hostname
- IP address
- Detailed type / function signatures
- Domain specific info

Should be replaced with generic commonly used words until the user asked in the prompt asked for the opposite.
