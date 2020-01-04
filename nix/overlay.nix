self: super:
{
  emacsPackages = super.emacsPackages.overrideScope' (_self: _super: {
    retrospect = self.callPackage ((import nix/sources.nix).retrospect) { pkgs = self; };
  });
}
