# -*- mode: sh; -*-

__emacs_echo () {
    if [[ -n $EMACS_ECHO ]]; then
        echo "$@"
    fi
}

__emacs_daemon_is_up () {
    test -S /tmp/emacs$(id --user)/server
}

__emacs_X_frame_exists () {
    xprop -name emacs_X_frame > /dev/null 2>&1
}

__emacs_daemon_start () {
    if ! $(__emacs_daemon_is_up); then
        __emacs_echo 'Starting emacs daemon'
        emacs --daemon --eval '(setq frame-title-format "emacs_X_frame")'
    else
        __emacs_echo 'Emacs daemon is already running'
    fi
}

__emacs_daemon_stop () {
    if $(__emacs_daemon_is_up); then
        __emacs_echo 'Stopping emacs daemon'
        emacsclient --eval '(progn (save-some-buffers t) (kill-emacs))'
    else
        __emacs_echo 'Emacs daemon is not running'
    fi
}

__emacs_daemon_restart () {
    __emacs_daemon_stop
    __emacs_daemon_start
}

__emacsclient_in_X_frame () {
    if $(__emacs_X_frame_exists); then
        __emacs_echo 'Attaching to existing X frame'
        emacsclient --no-wait "$@"
    else
        __emacs_echo 'Starting X frame'
        emacsclient --no-wait --create-frame "$@"
    fi
}

e () {
    __emacs_daemon_start
    __emacsclient_in_X_frame "$@"
}

ek () {
    __emacs_daemon_stop
}

er () {
    __emacs_daemon_restart
    __emacsclient_in_X_frame "$@"
}
