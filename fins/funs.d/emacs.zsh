# -*- mode: sh; -*-

function _emacs_echo () {
    if [[ -n $EMACS_ECHO ]]; then
        echo "$@"
    fi
}

function _emacs_daemon_is_up () {
    test -S /tmp/emacs$(id --user)/server
}

function _emacs_X_frame_exists () {
    xprop -name emacs_X_frame > /dev/null 2>&1
}

function _emacs_daemon_start () {
    if ! $(_emacs_daemon_is_up); then
        _emacs_echo 'Starting emacs daemon'
        emacs --daemon --eval '(setq frame-title-format "emacs_X_frame")'
    else
        _emacs_echo 'Emacs daemon is already running'
    fi
}

function _emacs_daemon_stop () {
    if $(_emacs_daemon_is_up); then
        _emacs_echo 'Stopping emacs daemon'
        emacsclient --eval '(progn (save-some-buffers t) (kill-emacs))'
    else
        _emacs_echo 'Emacs daemon is not running'
    fi
}

function _emacs_daemon_restart () {
    _emacs_daemon_stop
    _emacs_daemon_start
}

function _emacsclient_in_X_frame () {
    if $(_emacs_X_frame_exists); then
        _emacs_echo 'Attaching to existing X frame'
        emacsclient --no-wait "$@"
    else
        _emacs_echo 'Starting X frame'
        emacsclient --no-wait --create-frame "$@"
    fi
}

function e () {
    _emacs_daemon_start
    _emacsclient_in_X_frame "$@"
}

function ek () {
    _emacs_daemon_stop
}

function er () {
    _emacs_daemon_restart
    _emacsclient_in_X_frame "$@"
}
