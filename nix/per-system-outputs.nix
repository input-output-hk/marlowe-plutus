# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#35-nixper-system-outputsnix

{ inputs, inputs', pkgs, system, lib, haskellProjects, ... }:

{
  packages = {
    validators = pkgs.runCommand "validators" { } ''
      mkdir -p $out
      cd $out
      ${haskellProjects.default.hsPkgs.marlowe-plutus.components.exes.marlowe-validators}/bin/marlowe-validators
      rm *.tsv
    '';
  };
}
