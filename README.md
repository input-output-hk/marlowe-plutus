# Marlowe Plutus Validators

This project implements the on-chain component of the Cardano implementation of Marlowe as a Plutus smart contract.
The main outputs are the marlowe semantics validator, which checks the spending
of Marlowe script outputs, and the marlowe role payout validator, which checks
the spending of role payouts.

## Dev Shell

This repository uses nix to provide the development and build environment.

If you know nix, simply run `nix develop` to enter the shell; if prompted, trust 
the flake config values to enable access to our binary caches.   

The nix code is based on a template from the 
[IOGX flake](https://github.com/input-output-hk/iogx). 
For instructions on how to install and configure nix (including how to enable 
access to our binary caches), and how keep the flake inputs up to date, refer to 
[this section](https://github.com/input-output-hk/iogx/blob/main/doc/api.md#flakenix)
of the IOGX manual.

If you have direnv installed, you can have the shell automatically load and 
refresh for you by running these commands:

```bash
mkdir .direnv
direnv allow
```

Now, whenever you enter the repo the shell will be automatically loaded for you
and will be refreshed when the environment changes.

Once in the dev shell, type `info` to see the available commands and environment.

## Compiling the project

From the dev shell, you can compile the project with `cabal build all`.

Alternatively, you can compile with `nix` using `nix build .#marlowe-validators`

## Compiling the validators

You can compile the validators using the following command:

```bash
nix build .#marlowe-validators
```

This will build the project and run the `marlowe-validators` executable and
output the compiled plutus scripts into local directory called `result`. This
directory will contain two files:

- `marlowe-rolepayout.plutus` The compiled role payout validator as a JSON-encoded CBOR text-envelope.
- `marlowe-semantics.plutus` The compiled marlowe validator as a JSON-encoded CBOR text-envelope.
