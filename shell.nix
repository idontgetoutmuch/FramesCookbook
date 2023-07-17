let

myHaskellPackageOverlay = self: super: {
  myHaskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: rec {
      htoml = super.haskell.lib.dontCheck (hself.callCabal2nixWithOptions "htoml" (builtins.fetchGit {
        url = "https://github.com/mirokuratczyk/htoml";
        rev = "33971287445c5e2531d9605a287486dfc3cbe1da";
      }) "" { });
      Frames = super.haskell.lib.dontCheck (hself.callCabal2nixWithOptions "Frames" (builtins.fetchGit {
        url = "file:////Users/dom/Frames";
        rev = "00b8e26134327c622cf4939c07c9ebc2fca721c8";
      }) "" { });
      # flatparse = hsuper.haskellPackages.flatparse_0_4_1_0;
      # time = hself.callHackage "time" "1.12.1" {};
      # hasql = super.haskell.lib.dontCheck (hself.callHackage "hasql" "1.6" {});
      # hasql-transaction = super.haskell.lib.doJailbreak hsuper.hasql-transaction;
    };
  };
};

in

{ nixpkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixpkgs-22.11-darwin.tar.gz")
  {
    overlays = [ myHaskellPackageOverlay ];
    config.allowBroken = true;
  }
}:

let

  pkgs = nixpkgs;

  haskellDeps = ps: with ps; [
    base
    cassava
    lens
    microlens
    vinyl
    Frames
    hourglass
    microlens
    pretty-show
    singletons
    singletons-base
    singletons-th
    text
    turtle
  ];

in

pkgs.stdenv.mkDerivation {
  name = "rOnly";

  buildInputs = [
    pkgs.libintlOrEmpty
    (pkgs.myHaskellPackages.ghcWithPackages haskellDeps)
    pkgs.darwin.apple_sdk.frameworks.Cocoa
  ];
}
