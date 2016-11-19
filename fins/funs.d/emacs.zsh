# -*- mode: sh; -*-

function emacs_echo () {
    if [[ -n $EMACS_ECHO ]]; then
        echo "$@"
    fi
}

function emacs_daemon_is_up () {
    test -S /tmp/emacs$(id --user)/server
}

function emacs_X_frame_exists () {
    xprop -name emacs_X_frame > /dev/null 2>&1
}

function emacs_daemon_start () {
    if ! $(emacs_daemon_is_up); then
        emacs_echo 'Starting emacs daemon'
        emacs --daemon --eval '(setq frame-title-format "emacs_X_frame")'
    else
        emacs_echo 'Emacs daemon is already running'
    fi
}

function emacs_daemon_stop () {
    if $(emacs_daemon_is_up); then
        emacs_echo 'Stopping emacs daemon'
        emacsclient --eval '(progn (save-some-buffers t) (kill-emacs))'
    else
        emacs_echo 'Emacs daemon is not running'
    fi
}

function emacs_daemon_restart () {
    emacs_daemon_stop
    emacs_daemon_start
}

function emacsclient_in_X_frame () {
    if $(emacs_X_frame_exists); then
        emacs_echo 'Attaching to existing X frame'
        emacsclient --no-wait "$@"
    else
        emacs_echo 'Starting X frame'
        emacsclient --no-wait --create-frame "$@"
    fi
}

function e () {
    emacs_daemon_start
    emacsclient_in_X_frame "$@"
}

function ek () {
    emacs_daemon_stop
}

function er () {
    emacs_daemon_restart
    emacsclient_in_X_frame "$@"
}
