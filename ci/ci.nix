{ supportedSystems ? ["x86_64-linux"]
, supportedCompilers ? ["ghc802" "ghc822" "ghc841"]
}:

with (import <nixpkgs/pkgs/top-level/release-lib.nix> { inherit supportedSystems; });

let
  pkgs = import <nixpkgs> {};

  configurations = 
    pkgs.lib.listToAttrs (
      pkgs.lib.concatMap (compiler: 
        pkgs.lib.concatMap (system: 
          [{name = "haskell.packages." + compiler + ".sv." + system; value = {inherit compiler system;};}]
        ) supportedSystems
      ) supportedCompilers
    );

  jobs =
      pkgs.lib.mapAttrs (name: configuration: 
          let
            compiler = configuration.compiler; 
            system = configuration.system; 
            nixpkgs = { pkgs = pkgsFor system; };
            sv = import ../default.nix { inherit nixpkgs compiler; };
          in
            sv
      ) configurations;
in
  jobs
