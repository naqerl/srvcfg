# Responding to questions
When the user asks questions like "why", "what", "how", or any other informational question, treat it as a request for explanation or information. Do not interpret these questions as indirect requests to modify code or make changes. The user wants to understand reasons, decisions, and context - simply provide clear answers and explanations without making any modifications unless explicitly requested.

# Working in the project
All projects include root Makefile and may include some inner Makefiles in the subdirectories. You should ALWAYS use them to build, lint, compile, verify, deploy or test things. So when you need to do anything like that, LOOK for the existing Makefiles and if you do not find, what exactly is required, then propose modification or creation of the corrseponding Makefiles. Also Makefiles use `include` directive and source all required dotenv files, so you can run any target freely without seeing required env vars

# Knowledge
Use tavily MCP for web search for any info that should be fresh or unavailable locally like
- Documentation
- Best practise
- Existing solutions for general problem
