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
      rev = "0.3.2.1-nix";
      sha256 = "07f3nh1b4jvqq7lfyxp3ndgzap4dj31lbdjlgrjazrcd2h4zwdln";
    };

    validation = pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "validation";
      rev = "1";
      sha256 = "0bh3i6dkkxc6sxzbdwk9hkyyqm9cvx7261vl7zrxk0myrj2klfbr";
    };

    hedgehog = pkgs.fetchFromGitHub {
      owner  = "hedgehogqa";
      repo   = "haskell-hedgehog";
      rev    = "7858d626b198621bc674fbc235c7980fb4002f78";
      sha256 = "0mmypd6f3imh7bk6br9m9aj97k2yibz2bqcw3a5svp962zsjbkyp";
    };

  };

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      parsers = pkgs.haskell.lib.dontCheck super.parsers;
      separated = pkgs.haskell.lib.dontCheck (import sources.separated { inherit nixpkgs compiler; });
      validation= import sources.validation { inherit nixpkgs compiler; };
      hedgehog = super.callCabal2nix "hedgehog" "${sources.hedgehog}/hedgehog" {};
    };
  };

  sv = modifiedHaskellPackages.callPackage ./sv.nix {};

in
  sv
