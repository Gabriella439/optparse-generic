let
  config = {
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = pkgs.haskell.lib.packageSourceOverrides {
          optparse-generic = ./.;
        };
      };
    };
  };

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOs/nixpkgs/archive/3e09410da091230c725faa80af462557c94d11d9.tar.gz";

    sha256 = "089kf5sv6qqnry9cya9psjp0y65daiks2ypxmglm2wxs6c7zm4zr";
  };

  pkgs = import nixpkgs { inherit config; };

in
  { optparse-generic = pkgs.haskellPackages.optparse-generic;
  }
