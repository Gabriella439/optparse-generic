# You can build this repository using Nix by running:
#
#     $ nix-build -A optparse-generic release.nix
#
# You can also open up this repository inside of a Nix shell by running:
#
#     $ nix-shell -A optparse-generic.env release.nix
#
# ... and then Nix will supply the correct Haskell development environment for
# you
let
  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          optparse-generic = haskellPackagesNew.callPackage ./default.nix { };
        };
      };
    };
  };

  pkgs =
    import <nixpkgs> { inherit config; };

in
  { optparse-generic = pkgs.haskellPackages.optparse-generic;
  }
