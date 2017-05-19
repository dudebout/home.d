nix-ls () {
  ls --color=always /nix/store | sed -r 's/([a-z0-9]{32})-([-.a-zA-Z0-9]+)(.*)$/\2 (\1)\3/'
}

nix-which () {
  readlink $(which "$@")
}
