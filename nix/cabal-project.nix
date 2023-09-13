# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#33-nixcabal-projectnix

{ inputs, inputs', pkgs, system, lib, config, meta, ... }:

let
  packages = {
    # See https://github.com/input-output-hk/plutus/issues/1213 and
    # https://github.com/input-output-hk/plutus/pull/2865.
    marlowe.doHaddock = meta.enableHaddock;
    marlowe.flags.defer-plugin-errors = meta.enableHaddock;

    marlowe-plutus.ghcOptions = [ "-Werror" ];
  };

  modules = [{ inherit packages; }];

in

{
  inherit modules;
}
