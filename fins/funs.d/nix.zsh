nix-ls () {
    ls --color=always /nix/store | sed -r 's/([a-z0-9]{32})-([-.a-zA-Z0-9]+)(.*)$/\2 (\1)\3/'
}

nix-ag () {
    cd $HOME/.nix-defexpr/channels/nixpkgs
    ag "$@"
}

nix-upgrade-home.d () {
    file="${1:-https://github.com/NixOS/nixpkgs-channels/archive/nixos-19.09.tar.gz}"
    profile="${2:-default}"

    nix-env \
        --max-jobs auto \
        --file "$file" \
        --remove-all \
        --install \
        --from-expression \
        "import $HOME_D/nix"
}

nix-cleanup () {
    nix-env --delete-generations 7d
    nix-collect-garbage
}
