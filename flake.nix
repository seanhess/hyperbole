{
  description = "hyperbole overlay, development and hyperbole-demo";

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
    atomic-css.url = "github:seanhess/atomic-css";
  };

  outputs =
    {
      self,
      nixpkgs,
      nix-filter,
      flake-utils,
      pre-commit-hooks,
      atomic-css,
    }:
    let
      packageName = "hyperbole";
      demoName = "hyperbole-demo";
      src = nix-filter.lib {
        root = ./.;
        include = [
          "src"
          "client/dist"
          "client/util/live-reload.js"
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
          ghc9103 = (prev.overriddenHaskellPackages.ghc9103 or prev.haskell.packages.ghc9103).override (old: {
            overrides = prev.lib.composeExtensions (old.overrides or (_: _: { })) (
              hfinal: hprev: {
                "${packageName}" = hfinal.callCabal2nix packageName src { };
                # `atomic-css` upstream overlay does not support `ghc910x` currently
                atomic-css = hfinal.callCabal2nix "atomic-css" atomic-css { };
              }
            );
          });
          ghc984 = (prev.overriddenHaskellPackages.ghc984 or prev.haskell.packages.ghc984).override (old: {
            overrides = prev.lib.composeExtensions (old.overrides or (_: _: { })) (
              hfinal: hprev: {
                "${packageName}" = hfinal.callCabal2nix packageName src { };
              }
            );
          });
          # ghc967 = (prev.overriddenHaskellPackages.ghc967 or prev.haskell.packages.ghc967).override (old: {
          #   overrides = prev.lib.composeExtensions (old.overrides or (_: _: { })) (
          #     hfinal: hprev: {
          #       "${packageName}" = hfinal.callCabal2nix packageName src { };
          #     }
          #   );
          # });
        };
      };

    in
    {
      overlays.default = nixpkgs.lib.composeExtensions atomic-css.overlays.default overlay;
    }
    // flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
        };

        # DON'T include symlink `docs` (demo/docs -> ../docs)
        # as Nix (or `nix-filter`) can't handle it properly and will error instead
        # `docs` will be merged in `demo-docs-src` later on
        demo-src = nix-filter.lib {
          root = ./demo;
          include = [
            "App"
            "Example"
            (nix-filter.lib.matchExt "hs")
            ./demo/demo.cabal
          ];
        };

        docgen-src = nix-filter.lib {
          root = ./docs;
          include = [
            (nix-filter.lib.matchExt "hs")
            (nix-filter.lib.matchExt "md")
            ./docs/docgen.cabal
          ];
        };

        # Merges filtered `demo` + `docs` sources into `$out`.
        # Needed to solve `demo/docs` -> `../docs` symlink issue Nix has before.
        # Named "demo" so the store path is `/nix/store/<hash>-demo`.
        # That's what the `demo` expects to resolve source files. Check `demo/App/Docs/Snippet.hs` -> `localFile`
        demo-docs-src = pkgs.runCommand "demo" { } ''
          mkdir -p $out/docs
          cp -rL ${demo-src}/. $out/
          cp -rL ${docgen-src}/. $out/docs/
        '';

        ghcVersions = [
          # "967"
          "984"
          "9103"
        ];

        ghcPkgs = builtins.listToAttrs (
          map (ghcVer: {
            name = "ghc${ghcVer}";
            value = (
              pkgs.overriddenHaskellPackages."ghc${ghcVer}".extend (
                hfinal: hprev: {
                  ${demoName} = hfinal.callCabal2nix demoName demo-docs-src { };
                  docgen = hfinal.callCabal2nix "docgen" docgen-src { };
                }
              )
            );
          }) ghcVersions
        );

        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = src;
          excludes = [
            "^client/node_modules/"
            "^client/dist/"
          ];
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
            # `oxlint` uses the lockfile-pinned version here (`pre-commit-hooks` may provide a different one)
            oxlint = {
              enable = true;
              name = "oxlint";
              entry = "npm run --prefix client lint";
              language = "system";
              files = "^client/";
              pass_filenames = false;
            };
            # `oxfmt` uses the lockfile-pinned version here (`pre-commit-hooks` may provide a different one)
            oxfmt = {
              enable = true;
              name = "oxfmt (check)";
              entry = "npm run --prefix client fmt";
              language = "system";
              files = "^client/";
              pass_filenames = false;
            };
          };
        };

        shellCommon = version: {
          shellHook = ''
            ${pre-commit.shellHook}
            # `oxlint`/`oxfmt` hooks require lockfile-pinned binaries from `node_modules`
            if [ ! -d "client/node_modules" ]; then
              npm install --prefix client
            fi
          '';
          buildInputs = with pkgs.haskell.packages."ghc${version}"; [
            pkgs.nodejs_24
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
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ pkgs.zlib ];
        };

        exe =
          version:
          pkgs.haskell.lib.overrideCabal
            (pkgs.haskell.lib.justStaticExecutables self.packages.${system}."ghc${version}-${demoName}")
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
            name = demoName;
            created = "now";
            tag = "latest";
            copyToRoot = pkgs.buildEnv {
              name = "image-root";
              paths = [
                pkgs.tree
                pkgs.bash
                (exe version)
                (pkgs.runCommand "static-files" { } ''
                  mkdir -p $out/demo/static
                  mkdir -p $out/client/dist
                  cp -r ${./demo/static}/* $out/demo/static/
                  cp -r ${./client/dist}/* $out/client/dist
                '')
              ];
              pathsToLink = [
                "/bin"
                "/demo/static"
                "/client/dist"
              ];
            };
            config = {
              Entrypoint = [ "/bin/demo" ];
              WorkingDir = "/";
            };
          };
      in
      {
        # Rest of the output remains the same...
        checks = builtins.listToAttrs (
          map (version: {
            name = "ghc${version}-check-${demoName}";
            value = pkgs.runCommand "ghc${version}-check-demo" {
              buildInputs = [
                (exe version)
                # required to resolve `type docgen` in the check script
                (pkgs.haskell.lib.justStaticExecutables ghcPkgs."ghc${version}".docgen)
              ]
              ++ self.devShells.${system}."ghc${version}-shell".buildInputs;
            } "type demo; type docgen; CABAL_CONFIG=/dev/null cabal --dry-run repl; touch $out";
          }) ghcVersions
        );

        apps = {
          default = self.apps.${system}."ghc9103-${demoName}";
        }
        // builtins.listToAttrs (
          map (version: {
            name = "ghc${version}-${demoName}";
            value = {
              type = "app";
              program = "${exe version}/bin/demo";
            };
          }) ghcVersions
        );

        packages = {
          default = self.packages.${system}."ghc9103-${packageName}";
          docker = self.packages.${system}."ghc9103-docker";
        }
        // builtins.listToAttrs (
          builtins.concatMap (version: [
            {
              name = "ghc${version}-${demoName}";
              value = ghcPkgs."ghc${version}".${demoName};
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

        devShells = {
          default = self.devShells.${system}.ghc9103-shell;
        }
        // builtins.listToAttrs (
          map (version: {
            name = "ghc${version}-shell";
            value = ghcPkgs."ghc${version}".shellFor (
              shellCommon version
              // {
                packages = p: [
                  p.${packageName}
                  p.${demoName}
                ];
              }
            );
          }) ghcVersions
        );
      }
    );
}
