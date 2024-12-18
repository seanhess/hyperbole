{
  description = "hyperbole overlay, development and hyperbole-examples";

  inputs = {
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter/main";
    # FIXME
    web-view.url = "github:Skyfold/web-view";
  };

  outputs =
    {
      self,
      nixpkgs,
      nix-filter,
      flake-utils,
      pre-commit-hooks,
      web-view,
    }:
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
          packageOverrides = prev.lib.composeExtensions prev.haskell.packageOverrides (
            hfinal: hprev: {
              "${packageName}" = hfinal.callCabal2nix packageName src { };
            }
          );
          packages = prev.haskell.packages // {
            ghc982 = prev.haskell.packages.ghc982.override (old: {
              overrides = prev.lib.composeExtensions (old.overrides or (_: _: { })) (
                hfinal: hprev: {
                  http-api-data = hfinal.http-api-data_0_6_1;
                  uuid-types = hfinal.uuid-types_1_0_6;
                  effectful = hfinal.effectful_2_5_0_0;
                  effectful-core = hfinal.effectful-core_2_5_0_0;
                  scotty = hfinal.scotty_0_22;
                }
              );
            });
            ghc966 = prev.haskell.packages.ghc966.override (old: {
              overrides = prev.lib.composeExtensions (old.overrides or (_: _: { })) (
                hfinal: hprev: {
                  effectful = hfinal.effectful_2_5_0_0;
                  effectful-core = hfinal.effectful-core_2_5_0_0;
                  http-api-data = hfinal.http-api-data_0_6_1;
                  uuid-types = hfinal.uuid-types_1_0_6;
                }
              );
            });
          };
        };
      };
    in
    {
      overlays.default = nixpkgs.lib.composeExtensions web-view.overlays.default overlay;
    }
    // flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };

        pkgsOverlayed = import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
        };

        # Define GHC versions list
        ghcVersions = [
          "966"
          "982"
        ];

        # Create an attrset of GHC packages
        ghcPkgs = builtins.listToAttrs (
          map (version: {
            name = "ghc${version}";
            value = pkgsOverlayed.haskell.packages."ghc${version}";
          }) ghcVersions
        );

        shellCommon = version: {
          inherit (self.checks.${system}.pre-commit-check) shellHook;

          # Programs that will be available in the development shell
          buildInputs = with pkgs.haskell.packages."ghc${version}"; [
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

        example-src = nix-filter.lib {
          root = ./example;
          include = [
            (nix-filter.lib.inDirectory "Example")
            (nix-filter.lib.matchExt "hs")
            ./example/hyperbole-examples.cabal
            ./example/cabal.project
          ];
        };

        # Create examples for each GHC version
        examples = builtins.listToAttrs (
          map (version: {
            name = "ghc${version}-example";
            value = ghcPkgs."ghc${version}".callCabal2nix "example" example-src { };
          }) ghcVersions
        );

        examples-exe =
          version: pkgs.haskell.lib.justStaticExecutables self.packages.${system}."ghc${version}-example";

      in
      {
        checks =
          pkgs.lib.recursiveUpdate
            {
              pre-commit-check = pre-commit-hooks.lib.${system}.run {
                src = ./.;
                hooks = {
                  # hlint.enable = true;
                  # fourmolu.enable = true;
                  hpack.enable = true;
                  nixfmt-rfc-style.enable = true;
                  flake-checker = {
                    enable = true;
                    args = [ "--no-telemetry" ];
                  };
                  check-merge-conflicts.enable = true;
                };
              };
            }
            (
              builtins.listToAttrs (
                builtins.concatMap (version: [
                  {
                    name = "ghc${version}-check-${packageName}";
                    value = pkgs.runCommand "ghc${version}-check-${packageName}" {
                      buildInputs = [ self.packages.${system}."ghc${version}-${packageName}" ];
                    } "touch $out";
                  }
                  {
                    name = "ghc${version}-check-example";
                    value = pkgs.runCommand "ghc${version}-check-example" {
                      buildInputs = [ (examples-exe version) ];
                    } "type examples; touch $out";
                  }
                ]) ghcVersions
              )
            );

        apps =
          {
            default = self.apps.${system}.ghc966-example;
          }
          // builtins.listToAttrs (
            # Generate apps
            map (version: {
              name = "ghc${version}-example";
              value = {
                type = "app";
                program = "${examples-exe version}/bin/examples";
              };
            }) ghcVersions
          );
        inherit pkgs;

        packages =
          {
            default = self.packages.${system}."ghc982-${packageName}";
          }
          // builtins.listToAttrs (
            # Generate packages
            builtins.concatMap (version: [
              {
                name = "ghc${version}-${packageName}";
                value = ghcPkgs."ghc${version}".${packageName};
              }
              {
                name = "ghc${version}-example";
                value = examples."ghc${version}-example";
              }
            ]) ghcVersions
          );

        devShells =
          {
            default = self.devShells.${system}."ghc982-${packageName}";
          }
          // builtins.listToAttrs (
            # Generate devShells
            builtins.concatMap (version: [
              {
                name = "ghc${version}-${packageName}";
                value = ghcPkgs."ghc${version}".shellFor (
                  shellCommon version // { packages = p: [ p.${packageName} ]; }
                );
              }
              {
                name = "ghc${version}-example";
                value = ghcPkgs."ghc${version}".shellFor (
                  shellCommon version // { packages = _: [ examples."ghc${version}-example" ]; }
                );
              }
            ]) ghcVersions
          );
      }
    );
}
