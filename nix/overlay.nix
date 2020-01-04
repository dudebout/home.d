self: super:
{
  emacsPackages = super.emacsPackages.overrideScope' (_self: _super: {
    retrospect = self.callPackage ({ dash
                                   , fetchFromGitHub
                                   , lib
                                   , org-plus-contrib
                                   , melpaBuild }:
      melpaBuild {
        pname = "retrospect";
        ename = "retrospect";
        version = "0.1";
        src = (import nix/sources.nix).retrospect;
        recipe = builtins.toFile "retrospect-recipe" ''
          (retrospect
              :fetcher github
              :repo "dudebout/retrospect")
        '';
        packageRequires = [ dash org-plus-contrib ];
        meta = {
          homepage = https://github.com/dudebout/retrospect;
          license = lib.licenses.gpl3;
        };
      }) { inherit (self) lib fetchFromGitHub;
           inherit (self.emacsPackages) melpaBuild dash org-plus-contrib;
         };
  });
}
