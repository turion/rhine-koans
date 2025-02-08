{
  description = "Rhine-koans presentation";

  nixConfig = {
    extra-substituters = [
      "https://rhine-koans.cachix.org"
      "https://rhine.cachix.org"
    ];
    extra-trusted-public-keys = [
      "rhine-koans.cachix.org-1:cglDz0vWVge7HGENYsJRB6yU7+jZNXJyZ0Ud9Z0uW54="
      "rhine.cachix.org-1:oFsONI6lXn3XG4aVmIURDa2Rn0dW5XTPy6eJWROIs8k="
    ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable-small";

    nix-mkPandoc = {
      url = "github:chisui/nix-mkPandoc";
      flake = false;
    };

    rhine = {
      url = "github:turion/rhine";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, nix-mkPandoc, rhine }:
    with builtins;
    let
      lib = inputs.nixpkgs.lib;
      # All GHC versions that this project is tested with.
      # To be kept in sync with the `tested-with:` section in rhine-koans.cabal.
      # To do: Automated check whether this is the same as what get-tested returns.
      # Currently blocked on https://github.com/Kleidukos/get-tested/issues/39
      supportedGhcs = [
        "ghc92"
        "ghc94"
        "ghc96"
        "ghc98"
      ];
      # The Haskell packages set, for every supported GHC version
      hpsFor = pkgs:
        lib.genAttrs supportedGhcs (ghc: pkgs.haskell.packages.${ghc})
        // { default = pkgs.haskellPackages; };

      # A nixpkgs overlay containing everything defined in this repo, for reuse in downstream projects
      localOverlay = final: prev:
        let
          # A haskellPackages overlay containing everything defined in this repo
          haskellPackagesOverlay = hfinal: hprev: {
            rhine-koans = prev.haskell.lib.compose.enableCabalFlag "solution" (hfinal.callCabal2nix "rhine-koans" ./. { });
          };

          hps = hpsFor final;

          # Overrides that are necessary because of dependencies not being up to date or fixed yet in nixpkgs.
          # Check on nixpkgs bumps whether some of these can be removed.
          temporaryHaskellOverrides = with prev.haskell.lib.compose; [
            (hfinal: hprev: {
              time-domain = doJailbreak hprev.time-domain;
            })
          ];
        in
        {
          # The Haskell package set containing the packages defined in this repo
          haskell = prev.haskell // {
            packageOverrides = lib.composeManyExtensions ([
              prev.haskell.packageOverrides
              haskellPackagesOverlay
            ]
            ++ temporaryHaskellOverrides
            );
          };

          rhine-koans-all = prev.symlinkJoin {
            name = "rhine-koans-all";
            paths = map (hp: hp.rhine-koans) (attrValues hps);
          };
        };

      overlay = lib.composeManyExtensions [
        rhine.overlays.dependenciesOverlay
        localOverlay
      ];


      # Helper to build a flake output for all systems that are defined in nixpkgs
      forAllPlatforms = f:
        mapAttrs (system: pkgs: f system pkgs) inputs.nixpkgs.legacyPackages;

    in
    {
      # Reexport the overlay so other downstream flakes can use it to develop rhine-koans projects with low effort.
      overlays.default = overlay;

      # Usage: nix fmt
      formatter = forAllPlatforms (system: pkgs: pkgs.nixpkgs-fmt);

      # Build the presentation or rhine-koans executables on all GHCs, as well as docs and sdist
      # Usage:
      # - nix build
      # - nix build .#rhine-koans-all
      packages = forAllPlatforms (system: pkgs: {
        inherit (pkgs) rhine-koans-all;
        presentation = import ./presentation { inherit pkgs nix-mkPandoc; };
      } // lib.mapAttrs (ghcVersion: haskellPackages: haskellPackages.rhine-koans) (hpsFor (pkgs.extend overlay)));

      # We re-export the entire nixpkgs package set with our overlay.
      # Usage examples:
      # - nix build .#haskellPackages.rhine-koans
      # - nix build .#haskell.packages.ghc98.rhine-koans
      legacyPackages = forAllPlatforms (system: pkgs: pkgs.extend overlay);

      # Usage: nix develop (will use the default GHC)
      # Alternatively, specify the GHC: nix develop .#ghc98
      devShells = forAllPlatforms (systems: pkgs: mapAttrs
        (_: hp: hp.shellFor {
          packages = ps: [ ps.rhine-koans ];
          nativeBuildInputs = (with hp; [
            haskell-language-server
          ]) ++ (with pkgs; with haskellPackages; [
            cabal-gild
            cabal-install
            fourmolu
          ]);
        })
        (hpsFor (pkgs.extend overlay)));

      inherit supportedGhcs;
    };
}
