{
  description = "A flake for hyperbole development";

  inputs = {
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter/main";
    # web-view.url = "github:seanhess/web-view";
    web-view.url = "github:Skyfold/web-view";
  };

  outputs = { self, nixpkgs, nix-filter, flake-utils, pre-commit-hooks, web-view }:
    let
      packageName = "hyperbole";
      src = nix-filter.lib {
        root = ./.;
        include = [
          "src"
          "client/dist"
          "test"
          ./${packageName}.cabal
          ./cabal.project
          ./package.yaml
          ./fourmolu.yaml
          ./README.md
          ./CHANGELOG.md
          ./LICENSE

        ];
      };

      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = prev.lib.composeExtensions prev.haskell.packageOverrides
            (hfinal: hprev: {
              "${packageName}" = hfinal.callCabal2nix packageName src { };
            });
        };
      };
    in
    { overlays.default = nixpkgs.lib.composeExtensions web-view.overlays.default overlay; }
    // flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
        };

        example-src = nix-filter.lib {
          root = ./example;
          include = [
            (nix-filter.lib.inDirectory "Example")
            (nix-filter.lib.matchExt "hs")
            ./example/hyperbole-examples.cabal
            ./example/cabal.project
          ];
        };

        myHaskellPackages = pkgs.haskellPackages.override (old: {
          overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: { })) (hfinal: hprev: {
            Diff = hfinal.callHackage "Diff" "0.5" { };
            aeson = hfinal.callHackage "aeson" "2.2.2.0" { };
            attoparsec-aeson = hfinal.callHackage "attoparsec-aeson" "2.2.0.0" { };
            skeletest = hfinal.callHackage "skeletest" "0.1.0" { };
            effectful = hfinal.effectful_2_5_0_0;
            effectful-core = hfinal.effectful-core_2_5_0_0;
            # effectful = hfinal.callHackage "effectful" "2.4.0.0" { };
            # effectful-core = hfinal.callHackage "effectful-core" "2.4.0.0" { };
            # primitive = hfinal.callHackage "primitive" "0.9.0.0" { };
            http-api-data =
              pkgs.haskell.lib.doJailbreak (hfinal.callHackageDirect
                {
                  pkg = "http-api-data";
                  ver = "0.6.1";
                  sha256 = "sha256-WKXZcCW5+l4caZcZMG6gNFe6y4WMigit5w1WgShmeZk=";
                }
                { });
          });
        });
        shellCommon = {
          inherit (self.checks.${system}.pre-commit-check) shellHook;

          # Programs that will be available in the development shell
          buildInputs = with pkgs.haskellPackages; [
            pkgs.nodePackages_latest.webpack-cli
            cabal-install
            haskell-language-server
            fourmolu
            fast-tags
            ghcid
            pkgs.ghciwatch
            pkgs.hpack
          ];
          withHoogle = true;
          doBenchmark = true;

          CABAL_CONFIG = "/dev/null";
          # Ensure that libz.so and other libraries are available to TH
          # splices, cabal repl, etc.
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ pkgs.libz ];
        };

        selfPkgs = self.packages.${system};

      in
      {
        checks = {
          hyperbole-check = selfPkgs.${packageName};
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              hlint.enable = true;
              # uses 0.36.1 while hyperbole.cabal was made with 0.37.0
              # hpack.enable = true;
              fourmolu.enable = true;
              nixpkgs-fmt.enable = true;
            };
          };
        };

        packages = {
          default = selfPkgs.${packageName};
          ${packageName} = myHaskellPackages.${packageName};
        };

        devShells = {
          default = self.devShells.${system}.${packageName};
          ${packageName} = myHaskellPackages.shellFor (
            shellCommon // { packages = p: [ selfPkgs.${packageName} ]; }
          );
          example = myHaskellPackages.shellFor (
            shellCommon
            // {
              packages = _: [
                (myHaskellPackages.callCabal2nix "hyperbole-examples" example-src { })
              ];
            }
          );
        };

      });
}
