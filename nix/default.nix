nixpkgs:

let
  config = {
    allowUnfreePredicate = _pkg: true;
    pulseaudio = true;
  };
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
  expression = "${profile}/nix/default.nix";
  extra_pkgs =
    if builtins.pathExists expression
    then (import expression) pkgs
    else [];
in
  [
  ] ++ extra_pkgs
