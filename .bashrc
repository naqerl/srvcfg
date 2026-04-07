export GOPATH="$HOME/go"
export PNPM_HOME="$HOME/.local/share/pnpm"
export PATH="/usr/local/go/bin:$GOPATH/bin:$HOME/.local/bin:$PNPM_HOME:$PATH"
export ALTERNATE_EDITOR=""
export COLORTERM=truecolor

env_path="$HOME/.env"
test -f "$env_path" && source "$env_path"
