# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

force_color_prompt=yes

export GOPATH="$HOME/go"
export PNPM_HOME="$HOME/.local/share/pnpm"
export PATH="/usr/local/go/bin:$GOPATH/bin:$HOME/.local/bin:$PNPM_HOME:/home/user/.opencode/bin:$PATH"
export OPENCODE_EXPERIMENTAL_PLAN_MODE=1
export OPENCODE_ENABLE_EXA=false
export EDITOR=emacs

env_path="$HOME/.env"
test -f "$env_path" && source "$env_path"
[[ -f $(which fzf) ]] && eval "$(fzf --bash)"

if [[ -n "$SSH_CLIENT" && -z "$TMUX" && -z "$INSIDE_EMACS" ]]; then
    tmux new -A -D -s main
fi
