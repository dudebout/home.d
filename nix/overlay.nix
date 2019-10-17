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
          rev = "0d4b3abfbf2fbce9cac5f98a8a2bdd0597c932cb";
          sha256 = "098n8lq4q8nx62xzn8c26pkywxmm83n01d8wn6vqlglj9rbrz71k";
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
