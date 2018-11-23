{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      validation = pkgs.haskell.lib.dontCheck super.validation;
      hw-rankselect = self.callHackage "hw-rankselect" "0.12.0.4" {};
      sv-core = super.callCabal2nix "sv-core" ../sv-core {};
      concurrent-output = pkgs.haskell.lib.doJailbreak super.concurrent-output;
    };
  };

  sv = modifiedHaskellPackages.callPackage ./sv.nix {};

in
  sv
