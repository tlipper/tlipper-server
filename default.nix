let
  compiler = "ghc865";
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          amazonka-s3-streaming = haskellPackagesNew.callPackage amazonka-s3-streaming/amazonka-s3-streaming.nix {};
          tlipper-server = haskellPackagesNew.callPackage ./tlipper-server.nix {};
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };
in
  { tlipper-server = pkgs.haskellPackages.tlipper-server;

  tlipper-ui = pkgs.mkYarnPackage {
    name = "tlipper-ui";
    src = ../../React/tlipper-ui/.;
    packageJSON = ../../React/tlipper-ui/package.json;
    yarnLock = ../../React/tlipper-ui/yarn.lock;
    yarnNix = ../../React/tlipper-ui/yarn.nix;
  };
}

