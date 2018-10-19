nix-ls () {
  ls --color=always /nix/store | sed -r 's/([a-z0-9]{32})-([-.a-zA-Z0-9]+)(.*)$/\2 (\1)\3/'
}

nix-which () {
  readlink $(which "$@")
}

nx () {
    branch="$1"
    profile="$2"

    nix_dir=$HOME/nix

    nix-env --file $nix_dir/pkgs/$branch -p $nix_dir/profiles/$profile "${@:3}"
}

nix-upgrade-home.d () {
    nix-env --remove-all --install --from-expression "import $HOME/.config/nixpkgs/home.d.nix"
}
