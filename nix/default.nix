# Revisit if argument should be nixpkgs or { nixpkgs ? <nixpkgs> }
nixpkgs:

let
  config = { allowUnfreePredicate = pkg: true; pulseaudio = true; };
  home_d = builtins.getEnv "HOME_D";
  profile = home_d + "/profile";
  existing_overlays = dir:
    let
      overlay = dir + "/nix/overlay.nix";
    in
      if builtins.pathExists overlay
        then [(import overlay)]
        else [];
  overlays = existing_overlays home_d ++ existing_overlays profile;
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
