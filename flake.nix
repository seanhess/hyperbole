{
  description = "hyperbole overlay, development and hyperbole-examples";

  nixConfig = {
    extra-substituters = [
      "https://hyperbole.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hyperbole.cachix.org-1:9Pl9dJXuJrAxGkrG8WNQ/hlO9rKt9b5IPksG7y78UGQ="
    ];
  };

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
        overriddenHaskellPackages = {
          ghc982 = (prev.overriddenHaskellPackages.ghc982 or prev.haskell.packages.ghc982).override (old: {
            overrides = prev.lib.composeExtensions (old.overrides or (_: _: { })) (
              hfinal: hprev: {
                "${packageName}" = hfinal.callCabal2nix packageName src { };
                http-api-data = hfinal.http-api-data_0_6_1;
                uuid-types = hfinal.uuid-types_1_0_6;
                effectful = hfinal.effectful_2_5_0_0;
                effectful-core = hfinal.effectful-core_2_5_0_0;
                scotty = hfinal.scotty_0_22;
                data-default = hfinal.callHackage "data-default" "0.8.0.0" { };
              }
            );
          });
          ghc966 = (prev.overriddenHaskellPackages.ghc966 or prev.haskell.packages.ghc966).override (old: {
            overrides = prev.lib.composeExtensions (old.overrides or (_: _: { })) (
              hfinal: hprev: {
                "${packageName}" = hfinal.callCabal2nix packageName src { };
                effectful = hfinal.effectful_2_5_0_0;
                effectful-core = hfinal.effectful-core_2_5_0_0;
                http-api-data = hfinal.http-api-data_0_6_1;
                uuid-types = hfinal.uuid-types_1_0_6;
                data-default = hfinal.callHackage "data-default" "0.8.0.0" { };
              }
            );
          });
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
          overlays = [ self.overlays.default ];
        };

        example-src = nix-filter.lib {
          root = ./example;
          include = [
            "Example"
            (nix-filter.lib.matchExt "hs")
            ./example/${examplesName}.cabal
            "docgen"
          ];
        };

        ghcVersions = [
          "966"
          "982"
        ];

        ghcPkgs = builtins.listToAttrs (
          map (ghcVer: {
            name = "ghc${ghcVer}";
            value = (
              pkgs.overriddenHaskellPackages."ghc${ghcVer}".extend (
                hfinal: hprev: {
                  ${examplesName} = hfinal.callCabal2nix examplesName example-src { };
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
            hpack.enable = false;
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
          buildInputs = with pkgs.haskell.packages."ghc${version}"; [
            pkgs.nodePackages_latest.webpack-cli
            pkgs.nodePackages_latest.webpack
            pkgs.nodejs
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
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ pkgs.libz ];
        };

        exe =
          version:
          pkgs.haskell.lib.overrideCabal
            (pkgs.haskell.lib.justStaticExecutables self.packages.${system}."ghc${version}-${examplesName}")
            (drv: {
              # Added due to an issue building on macOS only
              postInstall = ''
                ${drv.postInstall or ""}
                  echo "Contents of $out/bin:"
                  ls -la $out/bin
                  echo remove-references-to -t ${ghcPkgs."ghc${version}".warp}
                  remove-references-to -t ${ghcPkgs."ghc${version}".warp} $out/bin/*
              '';
            });

        docker =
          version:
          pkgs.dockerTools.buildImage {
            name = examplesName;
            created = "now";
            tag = "latest";
            copyToRoot = pkgs.buildEnv {
              name = "image-root";
              paths = [
                pkgs.tree
                pkgs.bash
                (exe version)
                (pkgs.runCommand "static-files" { } ''
                  mkdir -p $out/example/static
                  mkdir -p $out/client/dist
                  cp -r ${./example/static}/* $out/example/static/
                  cp -r ${./client/dist}/* $out/client/dist
                '')
              ];
              pathsToLink = [
                "/bin"
                "/example/static"
                "/client/dist"
              ];
            };
            config = {
              Entrypoint = [ "/bin/examples" ];
              WorkingDir = "/";
            };
          };
      in
      {
        # Rest of the output remains the same...
        checks = builtins.listToAttrs (
          map (version: {
            name = "ghc${version}-check-${examplesName}";
            value = pkgs.runCommand "ghc${version}-check-example" {
              buildInputs = [
                (exe version)
              ] ++ self.devShells.${system}."ghc${version}-shell".buildInputs;
            } "type examples; type docgen; CABAL_CONFIG=/dev/null cabal --dry-run repl; touch $out";
          }) ghcVersions
        );

        apps =
          {
            default = self.apps.${system}."ghc966-${examplesName}";
          }
          // builtins.listToAttrs (
            map (version: {
              name = "ghc${version}-${examplesName}";
              value = {
                type = "app";
                program = "${exe version}/bin/examples";
              };
            }) ghcVersions
          );

        packages =
          {
            default = self.packages.${system}."ghc982-${packageName}";
            docker = self.packages.${system}."ghc982-docker";
          }
          // builtins.listToAttrs (
            builtins.concatMap (version: [
              {
                name = "ghc${version}-${examplesName}";
                value = ghcPkgs."ghc${version}".${examplesName};
              }
              {
                name = "ghc${version}-docker";
                value = docker version;
              }
              {
                name = "ghc${version}-${packageName}";
                value = ghcPkgs."ghc${version}".${packageName};
              }
            ]) ghcVersions
          );

        devShells =
          {
            default = self.devShells.${system}.ghc982-shell;
          }
          // builtins.listToAttrs (
            map (version: {
              name = "ghc${version}-shell";
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
