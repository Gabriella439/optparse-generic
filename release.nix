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

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOs/nixpkgs/archive/312a059bef8b29b4db4e73dc02ff441cab7bb26d.tar.gz";

    sha256 = "1j52yvkhw1inp6ilpqy81xv1bbwgwqjn0v9647whampkqgn6dxhk";
  };

  pkgs = import nixpkgs { inherit config; };

in
  { optparse-generic = pkgs.haskellPackages.optparse-generic;
  }
