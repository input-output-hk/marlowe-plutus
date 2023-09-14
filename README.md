# Marlowe Plutus Validators

This project implements the on-chain component of the Cardano implementation of Marlowe as a Plutus smart contract.
The main outputs are the marlowe semantics validator, which checks the spending
of Marlowe script outputs, and the marlowe role payout validator, which checks
the spending of role payouts.

## Dev Shell

To enter a dev shell with all dependencies installed, you can use

```bash
nix develop
```

Alternatively, if you have direnv installed, you can have the shell
automatically load and refresh for you by running these commands:

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
nix build .#validators
```

This will build the project and run the `marlowe-validators` executable and
output the compiled plutus scripts into local directory called `result`. This
directory will contain two files:

- `marlowe-rolepayout.plutus` The compiled role payout validator as a JSON-encoded CBOR text-envelope.
- `marlowe-semantics.plutus` The compiled marlowe validator as a JSON-encoded CBOR text-envelope.
