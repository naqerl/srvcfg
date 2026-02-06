# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

force_color_prompt=yes

export GOPATH="$HOME/go"
export PNPM_HOME="$HOME/.local/share/pnpm"
export PATH="/usr/local/go/bin:$GOPATH/bin:$HOME/.local/bin:$PNPM_HOME:$HOME/.opencode/bin:$HOME/.fzf/bin:$PATH"
export OPENCODE_EXPERIMENTAL_PLAN_MODE=1
export OPENCODE_ENABLE_EXA=false
export EDITOR=emacs
# Only set TERM if not already set (don't override tmux's TERM)
if [ -z "$TERM" ] || [ "$TERM" = "dumb" ]; then
    export TERM=xterm-ghostty
fi

env_path="$HOME/.env"
test -f "$env_path" && source "$env_path"
[[ -f $(which fzf) ]] && eval "$(fzf --bash)"

if [[ -n "$SSH_CLIENT" && -z "$TMUX" && -z "$INSIDE_EMACS" ]]; then
    tmux new -A -D -s main
fi

monit() {
    watch -c 'sudo netstat -tlp | grep "tcp\s"; echo; free -h; echo; df -h | grep "/dev/sda1\s"; echo; podman ps | tail -n +2; echo; ps -e -o pid,%cpu,%mem,args --sort=-%mem | head -n 10'
}

alias codex="npx @openai/codex"

