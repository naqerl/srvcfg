# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

force_color_prompt=yes

export GOPATH="$HOME/go"
export PNPM_HOME="$HOME/.local/share/pnpm"
export PATH="/usr/local/go/bin:$GOPATH/bin:$HOME/.local/bin:$PNPM_HOME:/home/user/.opencode/bin:$PATH"
export OPENCODE_ENABLE_EXA=true
export OPENCODE_EXPERIMENTAL_PLAN_MODE=1

env_path="$HOME/.env"
test -f "$env_path" && source "$env_path"

if [[ -n "$SSH_CLIENT" && -z "$TMUX" && -z "$INSIDE_EMACS" ]]; then
    tmux new -A -D -s main
fi
