# Revisit if argument should be nixpkgs or { nixpkgs ? <nixpkgs> }
nixpkgs:

let
  config = { allowUnfreePredicate = pkg: true; pulseaudio = true; };
  profile = builtins.getEnv "HOME_D" + "/profile/nix";
  overlay = "${profile}/overlay.nix";
  overlays =
  if builtins.pathExists overlay
    then [(import overlay)]
    else [];
  pkgs = nixpkgs { inherit config overlays; };
in

with pkgs;
let
  xmonad_packages = hPkgs : with hPkgs; [ xmonad-contrib ];
  expression = "${profile}/default.nix";
  extra_pkgs =
    if builtins.pathExists expression
    then (import expression) pkgs
    else [];
in
  [ (xmonad-with-packages.override { packages = xmonad_packages; })
    (import ./emacs.nix { inherit pkgs; })
  ] ++ extra_pkgs
