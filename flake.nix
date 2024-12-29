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
    web-view.url = "github:seanhess/web-view";
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
      examplesName = "hyperbole-examples";
      src = nix-filter.lib {
        root = ./.;
        include = [
          "src"
          "client/dist"
          "test"
          "example/Example"
          "example/docgen"
          "example/BulkUpdate.hs"
          "example/HelloWorld.hs"
          "example/Main.hs"
          "example/DevelMain.hs"
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

        example-src = nix-filter.lib {
          root = ./example;
          include = [
            "Example"
            (nix-filter.lib.matchExt "hs")
            ./example/${examplesName}.cabal
            # ./example/cabal.project
            "docgen"
          ];
        };

        # Define GHC versions list
        ghcVersions = [
          "966"
          "982"
        ];

        overridePkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
        };

        # Create an attrset of GHC packages
        ghcPkgs = builtins.listToAttrs (
          map (ghcVer: {
            name = "ghc${ghcVer}";
            value = (
              overridePkgs.haskell.packages."ghc${ghcVer}".extend (
                pkgs.haskell.lib.compose.packageSourceOverrides {
                  ${examplesName} = example-src;
                }
              )
            );
          }) ghcVersions
        );

        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = src;
          hooks = {
            hlint.enable = true;
            fourmolu.enable = true;
            hpack.enable = true;
            nixfmt-rfc-style.enable = true;
            flake-checker = {
              enable = true;
              args = [ "--no-telemetry" ];
            };
            check-merge-conflicts.enable = true;
          };
        };

        shellCommon = version: {
          inherit (pre-commit) shellHook;

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
            # pkgs.python312Packages.livereload
          ];
          withHoogle = true;
          doBenchmark = true;

          CABAL_CONFIG = "/dev/null";
          # Ensure that libz.so and other libraries are available to TH
          # splices, cabal repl, etc.
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ pkgs.libz ];
        };

        exe =
          version:
          pkgs.haskell.lib.justStaticExecutables self.packages.${system}."ghc${version}-${examplesName}";

      in
      {
        checks = builtins.listToAttrs (
          map (version: {
            name = "ghc${version}-check-example";
            value = pkgs.runCommand "ghc${version}-check-example" {
              buildInputs = [ (exe version) ];
            } "type examples; type docgen; touch $out";
          }) ghcVersions
        );

        apps =
          {
            default = self.apps.${system}.ghc966.example;
            docgen = self.apps.${system}.ghc966.docgen;
          }
          // builtins.listToAttrs (
            # Generate apps
            builtins.concatMap (version: [
              {
                name = "ghc${version}";
                value = {
                  example = {
                    type = "app";
                    program = "${exe version}/bin/examples";
                  };
                  docgen = {
                    type = "app";
                    program = "${exe version}/bin/docgen";
                  };
                };
              }
              {
                name = "ghc${version}-docgen";
                value = {
                  type = "app";
                  program = "${exe version}/bin/docgen";
                };
              }
            ]) ghcVersions
          );

        packages =
          {
            default = self.packages.${system}."ghc982-${examplesName}";
          }
          // builtins.listToAttrs (
            map (version: {
              name = "ghc${version}-${examplesName}";
              value = ghcPkgs."ghc${version}".${examplesName};
            }) ghcVersions
          );

        devShells =
          {
            default = self.devShells.${system}.ghc982;
          }
          // builtins.listToAttrs (
            # Generate devShells
            map (version: {
              name = "ghc${version}";
              value = ghcPkgs."ghc${version}".shellFor (
                shellCommon version
                // {
                  packages = p: [
                    p.${packageName}
                    p.${examplesName}
                  ];
                }
              );
            }) ghcVersions
          );
      }
    );
}
