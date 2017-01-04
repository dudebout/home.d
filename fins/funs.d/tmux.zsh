# -*- mode: sh; -*-

set -u

__session_exists () {
    local session_name="$1"

    tmux has-session -t "$session_name" 2> /dev/null
}

t () {
    local session_name="$1"


    if __session_exists "$session_name"; then
        tmux attach -t "$session_name"
    else
        tmux new-session -s "$session_name"
    fi
}

xt () {
    local session_name="${1:-default}"
    local xterm_title="${2:-tmux:$session_name}"

    xterm -title "$xterm_title" -e t "$session_name"
}


ct () {
    local directory="$1"
    local session_name=default

    if __session_exists "$session_name"; then
        (cd "$directory"; tmux new-window -t "$session_name")
        # Without this sleep, the xdotool does not focus on the existing window
        sleep 0.1
    else
        xt &
        while ! __session_exists "$session_name"; do
            sleep 0.01
        done
        (cd "$directory"; tmux new-window -t "$session_name")
        tmux kill-window -t "$session_name:1"
        tmux move-window -s "$session_name:2" -t "$session_name:1"
    fi
    xdotool key 'Super_R+t'
}
