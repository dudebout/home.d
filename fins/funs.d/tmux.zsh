# -*- mode: sh; -*-

t () {
    local session_name="${1:-default}"

    tmux attach -t "$session_name" || tmux new-session -s "$session_name"
}

xt () {
    local session_name="${1:-default}"
    local xterm_title="${2:-tmux:$session_name}"

    xterm -title "$xterm_title" -e t "$session_name"
}
