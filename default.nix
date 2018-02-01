{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  sources = {
    separated = pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "separated";
      rev = "0.3.0";
      sha256 = "0qmm0bmnkv0dbdncb3f917zdabw3saqy1mrn0ihx0m0kl9v6hdld";
    };

    validation = pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "validation";
      rev = "0.6.2";
      sha256 = "02fabbgsbn47gh794alnfkrfs1nh9rpwc7dqig6bq9sx7v26j2qf";
    };
  };

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      parsers = pkgs.haskell.lib.dontCheck super.parsers;
      separated = import sources.separated { inherit nixpkgs compiler; };
      validation= import sources.validation { inherit nixpkgs compiler; };
    };
  };

  sv = modifiedHaskellPackages.callPackage ./sv.nix {};

in
  sv
