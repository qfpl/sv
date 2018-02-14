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
      rev = "aa8d2fa835b66b191ff1b150c58c56117e1c7e7c";
      sha256 = "15n3wc7l1wjfs5ibhxz41r19asdb9ga6scpfalds7kp2ypp96shb";
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
