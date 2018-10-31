nix-ls () {
    ls --color=always /nix/store | sed -r 's/([a-z0-9]{32})-([-.a-zA-Z0-9]+)(.*)$/\2 (\1)\3/'
}

nix-ag () {
    cd $HOME/.nix-defexpr/channels/nixpkgs
    ag "$@"
}

nix-upgrade-home.d () {
    file="${1:-https://github.com/NixOS/nixpkgs-channels/archive/nixos-18.09.tar.gz}"

    nix-env \
        --file "$file" \
        --remove-all \
        --install \
        --from-expression \
        "import $HOME_D/nix"
}
