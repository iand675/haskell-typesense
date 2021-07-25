let 
  sources = import ./nix/sources.nix {};
  pkgs = import sources.nixpkgs {};
  haskellNix = import sources.haskellNix {};
  haskellOverlayPkgs = import
    # haskell.nix provides access to the nixpkgs pins which are used by our CI,
    # hence you will be more likely to get cache hits when using these.
    # But you can also just use your own, e.g. '<nixpkgs>'.
    haskellNix.sources.nixpkgs-unstable
    # These arguments passed to nixpkgs, include some patches and also
    # the haskell.nix functionality itself as an overlay.
    haskellNix.nixpkgsArgs;
in (import ./default.nix).shellFor {
  tools = {
    # cabal = "3.2.0.0";
    # stack = "2.7.1";
    hlint = "latest";
    haskell-language-server = "latest";
  };
  shellHook = ''
    ${(import ./git-hooks.nix).pre-commit-check.shellHook}
  '';
  buildInputs = [
    pkgs.git
    pkgs.stack
    # TODO is there an okay way to derive this from the pkgSet in default.nix?
    # haskellOverlayPkgs.haskell-nix.nix-tools.ghc8105
  ];
}

