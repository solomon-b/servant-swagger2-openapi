{
  description = "Servant Example App";

  inputs = {
    # Nix Inputs
    nixpkgs.url = github:nixos/nixpkgs/23.05;
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = { self , nixpkgs , flake-utils ,}:
    let
      utils = flake-utils.lib;
    in
    utils.eachDefaultSystem (system:
    let
      compilerVersion = "ghc945";
      pkgs = nixpkgs.legacyPackages.${system};
      hsPkgs = pkgs.haskell.packages.${compilerVersion}.override {
        overrides = hfinal: hprev: {
          servant-example = hfinal.callCabal2nix "servant-example" ./. { };
        };
      };
    in
    rec {
      packages =
        utils.flattenTree
          { servant-example = hsPkgs.servant-example; };

      # nix develop
      devShell = hsPkgs.shellFor {
        packages = p: [
          p.servant-example
        ];
        buildInputs = with pkgs;
          [
            hsPkgs.haskell-language-server
            haskellPackages.cabal-install
            cabal2nix
            haskellPackages.ghcid
            haskellPackages.fourmolu
            haskellPackages.cabal-fmt
            nixpkgs-fmt
            zlib
          ];
      };

      # nix build
      defaultPackage = packages.servant-example;
    });
}
