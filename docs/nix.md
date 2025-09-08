Usage with NIX
==============


How to Import Flake
-------------------

You can import this flake's overlay to add `hyperbole` to all package sets and override ghc966 and ghc982 with the packages to satisfy `hyperbole`'s dependencies.

```nix
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    hyperbole.url = "github:seanhess/hyperbole"; # or "path:/path/to/cloned/hyperbole";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, hyperbole, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ hyperbole.overlays.default ];
        };
        haskellPackagesOverride = pkgs.overriddenHaskellPackages.ghc966.override (old: {
          overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: { })) (hfinal: hprev: {
            # your overrides here
          });
        });
      in
      {
        devShells.default = haskellPackagesOverride.shellFor {
          packages = p: [ p.hyperbole ];
        };
      }
    );
}
```


Local Development with NIX
--------------------------

### Recommended ghcid command

If you want to work on both the hyperbole library and example code, this `ghcid` command will run (and hot reload) the examples server as you change any non-testing code.

```
ghcid --setup=Example.App.update --command="cabal repl exe:examples lib:hyperbole" --run=Example.App.update --warnings --reload=./client/dist/hyperbole.js
```

If you want to work on the test suite, this will run the tests each time any library code is changed.

```
ghcid --command="cabal repl test lib:hyperbole" --run=Main.main --warnings --reload=./client/dist/hyperbole.js
```

### Nix

- `nix flake check` will build the library, example executable and devShell with ghc-9.8.2 and ghc-9.6.6
    - This is what the CI on GitHub runs
- `nix run` or `nix run .#ghc982-example` to start the example project with GHC 9.8.2
    - `nix run .#ghc966-example` to start the example project with GHC 9.6.6
- `nix develop` or `nix develop .#ghc982-shell` to get a shell with all dependencies installed for GHC 9.8.2. 
    - `nix develop .#ghc966-shell` to get a shell with all dependencies installed for GHC 9.6.6. 
- `nix build`, `nix build .#ghc982-hyperbole` and `nix build .#ghc966-hyperbole` builds the library with the `overriddenHaskellPackages`
    - If you want to import this flake, use the overlay
- `nix flake update atomic-css` will update the `atomic-css` input

Note: You can always run `cachix use hyperbole` to use the GitHub CI populated cache if you didn't allow adding 'extra-substituters' when first using this flake.

### Common Nix Issues

#### Not Allowed to Refer to GHC

If you get an error like:

```
error: output '/nix/store/64k8iw0ryz76qpijsnl9v87fb26v28z8-my-haskell-package-1.0.0.0' is not allowed to refer to the following paths:
         /nix/store/5q5s4a07gaz50h04zpfbda8xjs8wrnhg-ghc-9.6.3
```

Follow these [instructions](https://nixos.org/manual/nixpkgs/unstable/#haskell-packaging-helpers)

#### Dependencies Incorrect

If you need to update `atomic-css` run `nix flake update atomic-css`, otherwise:

You will need to update the overlay, look for where it says `"${packageName}" = hfinal.callCabal2nix packageName src { };` and add a line like `Diff = hfinal.callHackage "Diff" "0.5" { };` with the package and version you need.

#### Missing Files

Check the `include` inside the `nix-filter.lib` to see if all files needed by cabal are there.


