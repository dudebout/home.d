__emacs_echo () {
    if [[ -v ${EMACS_ECHO} ]]; then
        echo "$@"
    fi
}

__emacs_daemon_socket () {
    echo /tmp/emacs$(id --user)/server
}

__emacs_daemon_socket_exists () {
    test -S $(__emacs_daemon_socket)
}

__emacs_daemon_is_up () {
    __emacs_daemon_socket_exists && lsof $(__emacs_daemon_socket) > /dev/null
}

__emacs_X_frame_exists () {
    xprop -name emacs_X_frame > /dev/null 2>&1
}

__emacs_daemon_start () {
    if ! $(__emacs_daemon_is_up); then
        if $(__emacs_daemon_socket_exists); then
            __emacs_echo 'Removing stale socket'
            rm -f $(__emacs_daemon_socket)
        fi
        __emacs_echo 'Starting emacs daemon'
        (cd $HOME && emacs --daemon --eval '(setq frame-title-format "emacs_X_frame")')
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
        emacsclient "$@"
    else
        __emacs_echo 'Starting X frame'
        emacsclient --create-frame "$@"
    fi
}

__emacsclient_in_X_frame_no_wait () {
    __emacsclient_in_X_frame --no-wait "$@"
}


emacs-attach () {
    __emacs_daemon_start
    __emacsclient_in_X_frame_no_wait "$@"
}

emacs-wait () {
    __emacs_daemon_start
    __emacsclient_in_X_frame "$@"
}

emacs-kill () {
    __emacs_daemon_stop
}

emacs-restart () {
    __emacs_daemon_restart
    __emacsclient_in_X_frame_no_wait "$@"
}

magit () {
    (if [[ $# == 1 ]]; then
         cd $1;
     fi
     __emacs_daemon_start
     __emacsclient_in_X_frame_no_wait --eval '(magit-status)'
    )
}
