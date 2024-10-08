let haskell-nix = import ./dep/haskell.nix {};
    pkgs = import haskell-nix.sources.nixpkgs haskell-nix.nixpkgsArgs;
in pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "haskell-nix-project";
    src = ./.;
  };

  compiler-nix-name = "ghc982";
}
