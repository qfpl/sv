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
      rev = "0db19e31c20b8456c06163338d9b94841faa4049";
      sha256 = "1hil4kqknc6qmd5a6nlxdb6yfm5l7rf1pqdjkl6m0bxz6d6r5za5";
    };
  };

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      parsers = pkgs.haskell.lib.dontCheck super.parsers;
      separated = import sources.separated { inherit nixpkgs compiler; };
    };
  };

  sv = modifiedHaskellPackages.callPackage ./sv.nix {};

in
  sv
