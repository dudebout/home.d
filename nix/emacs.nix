{ pkgs }:

with pkgs;

emacsPackages.emacsWithPackages (ePkgs: with ePkgs;
  [ use-package
    company-ghci
    # Not easy to set up
    # dante
    # attrap
    retrospect
    intero
    flycheck
    flycheck-haskell
    haskell-mode
    nix-sandbox
    ace-window
    auth-source-pass
    avy
    agda2-mode
    company
    # Could not get it to work for haskell
    # company-quickhelp
    company-coq
    counsel
    counsel-projectile
    elisp-slime-nav
    fill-column-indicator
    git-gutter
    git-timemachine
    god-mode
    helpful
    hydra
    iedit
    ivy
    ivy-hydra
    jinja2-mode
    lispy
    macrostep
    magit
    markdown-mode
    multiple-cursors
    nix-buffer
    nix-mode
    org-pdfview
    org-plus-contrib
    org-trello
    paredit
    pass
    pdf-tools
    projectile
    proofgeneral
    rainbow-mode
    rust-mode
    selected
    smart-mode-line
    swiper
    wgrep
    which-key
    yaml-mode
    zenburn-theme

    pkgs.git

    # This is wrong. The version of GHC used by emacs should depend on the
    # project being built, not on the version of emacs being installed.
    pkgs.haskellPackages.stylish-haskell
    pkgs.haskellPackages.hlint
    pkgs.haskellPackages.ghc

    ggtags
  ]
)
