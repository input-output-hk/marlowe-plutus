{ repoRoot, inputs, pkgs, lib, system }:

let

  cabalProject = pkgs.haskell-nix.cabalProject' ({ config, pkgs, ... }: {
    name = "marlowe-plutus";
    src = ../.;
    compiler-nix-name = "ghc8107";
    shell.withHoogle = false;
    inputMap = {
      "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.iogx.inputs.CHaP;
    };
    modules = [{
      packages = {
        marlowe-plutus.ghcOptions = [ "-Werror" ];
      };
    }];
  });

  project = lib.iogx.mkHaskellProject {
    inherit cabalProject;
    shellArgs = repoRoot.nix.shell;
  };

in

project
