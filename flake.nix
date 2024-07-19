{
  description = "A flake for hyperbole development";

  inputs = {
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, pre-commit-hooks }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        packageName = "hyperbole";
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages.override {
          overrides = self: super: {
            # Tests for web-view version 0.4.0 are broken, so we disable them
            web-view =
              pkgs.haskell.lib.dontCheck (super.callHackageDirect
                {
                  pkg = "web-view";
                  ver = "0.4.0";
                  sha256 = "sha256-ug5pduRQEfgcKRCDNP/UmGS3jGNVKlSPrDk06Dp6sYc=";
                }
                { });

            http-api-data =
              pkgs.haskell.lib.doJailbreak (super.callHackageDirect
                {
                  pkg = "http-api-data";
                  ver = "0.6.1";
                  sha256 = "sha256-WKXZcCW5+l4caZcZMG6gNFe6y4WMigit5w1WgShmeZk=";
                }
                { });

          };
        };
      in
      {
        packages.default = self.packages.${system}.${packageName};
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self { };

        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              # TODO: hlint is currently failing. Fix hints.
              # hlint.enable = true;
              hpack.enable = true;
              fourmolu.enable = true;
              nixpkgs-fmt.enable = true;
            };
          };
        };

        devShells.default = pkgs.haskellPackages.shellFor rec {
          inherit (self.checks.${system}.pre-commit-check) shellHook;

          packages = p: [ self.packages.${system}.${packageName} ];

          # Programs that will be available in the development shell
          buildInputs = with pkgs; [
            nodePackages_latest.webpack-cli
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            haskellPackages.fourmolu
            ghciwatch
            hpack
          ];

          # Ensure that libz.so and other libraries are available to TH
          # splices, cabal repl, etc.
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;
        };
      });
}
