__emacs-echo () {
    # only works since zsh 5.3
    # if [[ -v EMACS_ECHO ]]; then
    # would also work
    # if [[ -n ${EMACS_ECHO+x} ]]; then
    # man zshexpn(1)
    #    ${+name}
    #        If name is the name of a set parameter `1' is substituted, otherwise
    #        `0' is substituted.
    if (( ${+EMACS_ECHO} )); then
        echo "$@"
    fi
}

__emacs-daemon-socket () {
    echo /tmp/emacs$(id --user)/server
}

__emacs-daemon-socket-exists () {
    test -S $(__emacs-daemon-socket)
}

__emacs-daemon-socket-is-open () {
    # FIXME
    __emacs-echo 'Skip the socket open test because it takes 2s'
    true || lsof -w $(__emacs-daemon-socket) > /dev/null
}

__emacs-daemon-is-up () {
    __emacs-daemon-socket-exists && __emacs-daemon-socket-is-open
}

# FIXME make the name a variable in .home.d
__emacs-X-frame-exists () {
    xprop -name emacs_X_frame > /dev/null 2>&1
}

__emacs-daemon-start () {
    if ! $(__emacs-daemon-is-up); then
        if $(__emacs-daemon-socket-exists); then
            __emacs-echo 'Removing stale socket'
            rm -f $(__emacs-daemon-socket)
        fi
        __emacs-echo 'Starting emacs daemon'
        (cd $HOME && emacs --daemon --eval '(setq frame-title-format "emacs_X_frame")')
    else
        __emacs-echo 'Emacs daemon is already running'
    fi
}

__emacs-daemon-stop () {
    if $(__emacs-daemon-is-up); then
        __emacs-echo 'Stopping emacs daemon'
        emacsclient --eval '(progn (save-some-buffers t) (kill-emacs))'
    else
        __emacs-echo 'Emacs daemon is not running'
    fi
}

__emacs-daemon-restart () {
    __emacs-daemon-stop
    __emacs-daemon-start
}

__emacsclient () {
    # FIXME: it does not always work
    emacsclient --eval '(define-key input-decode-map [?\C-m] [C-m])'
    emacsclient "$@"
}

__emacsclient-in-X-frame () {
    if $(__emacs-X-frame-exists); then
        __emacs-echo 'Attaching to existing X frame'
        __emacsclient "$@"
    else
        __emacs-echo 'Starting X frame'
        __emacsclient --create-frame "$@"
    fi
}

__emacsclient-in-X-frame-no-wait () {
    __emacsclient-in-X-frame --no-wait "$@"
}


emacs-attach () {
    __emacs-daemon-start
    __emacsclient-in-X-frame-no-wait "$@"
}

emacs-wait () {
    __emacs-daemon-start
    __emacsclient-in-X-frame "$@"
}

emacs-kill () {
    __emacs-daemon-stop
}

emacs-restart () {
    __emacs-daemon-restart
    __emacsclient-in-X-frame-no-wait "$@"
}

magit () {
    (if [[ $# == 1 ]]; then
         cd $1;
     fi
     __emacs-daemon-start
     __emacsclient-in-X-frame-no-wait --eval '(magit-status)'
    )
}
