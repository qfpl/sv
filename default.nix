{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      tasty-discover = pkgs.haskell.lib.dontCheck super.tasty-discover_3_0_2;
    };
  };

  separated-values = modifiedHaskellPackages.callPackage ./separated-values.nix {};

in
  separated-values
