{ repoRoot, inputs, pkgs, lib, system }:

let

  cabalProject = pkgs.haskell-nix.cabalProject' ({ config, pkgs, ... }: {
    name = "marlowe-plutus";
    src = ../.;
    compiler-nix-name = "ghc928";
    shell.withHoogle = false;
    inputMap = {
      "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.iogx.inputs.CHaP;
    };
    modules = [{
      packages = {
        marlowe-plutus.ghcOptions = [ "-Werror" ];
      };
    }];
    flake.variants.traced = {
      modules = [{
        packages = {
          marlowe-plutus = {
            ghcOptions = [ "-Werror" ];
            configureFlags = [ "--flag=+trace-plutus" ];
          };
        };
      }];
    };
  });

  project = lib.iogx.mkHaskellProject {
    inherit cabalProject;
    shellArgs = repoRoot.nix.shell;
  };

in

project
