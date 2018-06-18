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
      rev    = "0.6";
      sha256 = "101bxgnxdmjg6x5jdjgbzayb747lxv8yv28bjg0kr6xw4kqi8kpw";
    };

    tasty-hedgehog = pkgs.fetchFromGitHub {
      owner  = "qfpl";
      repo   = "tasty-hedgehog";
      rev    = "9797ca980e547c160b5e9e3f07d7b0d1d5c40fee";
      sha256 = "039r8hay6cyq762ajn89nj4bfgz50br15x4nkracw3kzdyikn5xh";
    };

    hw-dsv = pkgs.fetchFromGitHub {
      owner  = "haskell-works";
      repo   = "hw-dsv";
      rev    = "v0.2";
      sha256 = "07r9d2hxl1wfwnxlg2sffh1kw13xh8xcfb3ml7cyys4rahhzg56v";
    };

    hw-hspec-hedgehog = pkgs.fetchFromGitHub {
      owner  = "haskell-works";
      repo   = "hw-hspec-hedgehog";
      rev    = "v0.1.0.5";
      sha256 = "1hmpa31j29nk0yh2xs5lcbdlj7rpyyimagmwk80b8qbj4kmpzyzs";
    };

    hw-prim = pkgs.fetchFromGitHub {
      owner  = "haskell-works";
      repo   = "hw-prim";
      rev    = "v0.6.2.0";
      sha256 = "02m76y4h75gsm26nsc0xy88c39k75by5dhp9gmqqm1fkdy0p5c3n";
    };

    hw-rankselect = pkgs.fetchFromGitHub {
      owner  = "haskell-works";
      repo   = "hw-rankselect";
      rev    = "v0.12.0.3";
      sha256 = "0l14300yjy6b5p27f1x6iljsq5ph874wnzfz0wszby8cg3r918xh";
    };

    hw-rankselect-base = pkgs.fetchFromGitHub {
      owner  = "haskell-works";
      repo   = "hw-rankselect-base";
      rev    = "v0.3.2.0";
      sha256 = "183czzmhwb9l0v9cw0g50dzph87n63lz08lhk7hqj636d1ks6ng9";
    };

    bits-extra = pkgs.fetchFromGitHub {
      owner  = "haskell-works";
      repo   = "bits-extra";
      rev    = "v0.0.1.3";
      sha256 = "0fwhsvx8rj4pj8f6nwv9q34r247j7j9ccwkqlk0009ayvcfrz34v";
    };
  };

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      parsers = pkgs.haskell.lib.dontCheck super.parsers;
      separated = pkgs.haskell.lib.dontCheck (import sources.separated { inherit nixpkgs compiler; });
      validation= import sources.validation { inherit nixpkgs compiler; };
      hedgehog = super.callCabal2nix "hedgehog" "${sources.hedgehog}/hedgehog" {};
      tasty-hedgehog = super.callCabal2nix "tasty-hedgehog" "${sources.tasty-hedgehog}" {};
      hw-dsv = super.callCabal2nix "hw-dsv" "${sources.hw-dsv}" {};
      hw-hspec-hedgehog = super.callCabal2nix "hw-hspec-hedgehog" "${sources.hw-hspec-hedgehog}" {};
      hw-prim = super.callCabal2nix "hw-prim" "${sources.hw-prim}" {};
      hw-rankselect = pkgs.haskell.lib.dontCheck (super.callCabal2nix "hw-rankselect" "${sources.hw-rankselect}" {});
      hw-rankselect-base = super.callCabal2nix "hw-rankselect-base" "${sources.hw-rankselect-base}" {};
      bits-extra = super.callCabal2nix "bits-extra" "${sources.bits-extra}" {};
      sv-common = super.callCabal2nix "sv-common" ./sv-common {};
    };
  };

  sv = modifiedHaskellPackages.callPackage ./sv.nix {};

in
  sv
