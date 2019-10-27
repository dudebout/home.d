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
        src = fetchFromGitHub {
          owner = "dudebout";
          repo = "retrospect";
          rev = "7ef5782919c74ca872473d8a3ad6c2cb2e30d1c2";
          sha256 = "182sbrzkps425v8fd9yaf4yjpc2hr1jhx37p3m5rwa2m441ijclh";
        };
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
