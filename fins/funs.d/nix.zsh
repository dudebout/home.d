nix-ls () {
    ls --color=always /nix/store | sed -r 's/([a-z0-9]{32})-([-.a-zA-Z0-9]+)(.*)$/\2 (\1)\3/'
}

nix-ag () {
    cd $HOME/.nix-defexpr/channels/nixpkgs
    ag "$@"
}

nix-upgrade-home.d () {
    nix-env --file '<nixos-18_09>' --remove-all --install --from-expression "import $HOME/.config/nixpkgs/home.d.nix"
}
