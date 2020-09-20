emacs-attach () {
    if emacsclient --no-wait --eval '(> (length (frame-list)) 1)' | grep --quiet '^t$'; then
        if [[ $# -eq 0 ]]; then
            set -- .
        fi
        emacsclient --no-wait "$@"
    else
        emacsclient --no-wait --create-frame "$@"
    fi
}

magit () {
    (if [[ $# == 1 ]]; then
         cd $1;
     fi
     emacs-attach --eval '(magit-status)'
    )
}
