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

          optparse-applicative =
            haskellPackagesNew.optparse-applicative_0_16_0_0;
        };
      };
    };
  };

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOs/nixpkgs/archive/d8e0ade97ad89cd7ea4452e41b4abcaf7e04a8b7.tar.gz";

    sha256 = "1rm6z9cch0kld1742inpsch06n97qik30a3njglvq52l8g9xw2jj";
  };

  pkgs = import nixpkgs { inherit config; };

in
  { optparse-generic = pkgs.haskellPackages.optparse-generic;
  }
