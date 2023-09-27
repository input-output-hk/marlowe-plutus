# Plutus bridge from Charli3 oracles to Marlowe contracts

The Charli3 oracle bridge is a Plutus V2 script that verifies that a Charli3 oracle's reference input datum is correctly embodied into an `IChoice` action for a Marlowe contract. ***This Plutus script is experimental and has not been audited.***


## Parameters

The bridge validator takes three parameters:

1. The currency symbol for the token held in the Charli3 oracle's reference UTXO.
2. The token name for the token held in the Charli3 oracle's reference UTXO.
3. The name of the Marlowe `Choice` where the Marlowe contract will receive oracle input.


## Datum

The datum consist of a pair of items.

1. The public key hash for the operator of the oracle.
2. The token name for the thread role token held by the Marlowe contract.


## Redeemer

The redeemer is a boolean.

1. `True` if the oracle should verify the consistency of the Marlowe input with the Charli3 oracle and then continue operating.
2. `False` if the oracle should terminate.


## UTXO

The bridge's UTXO must contain the role token for the Marlowe party making the `Choice` action for the oracle. It may not contain other tokens, aside from ada.


## Transaction

The transactions where the bridge is used to ensure consistency between a Charli3 oracle feed and a Marlowe `Choice` must consist of the following:

1. The Charli3 reference input containing is oracle feed token and the price data.
2. The Marlowe UTXO and `IChoice` action for receiving the price.
3. The bridge UTXO where datum remains unchanged before/after the transaction, and with a redeemer of `True`.


## Creating the script and computing its hash.

Simply run the executable program with the three oracle parameters and output file name as arguments.

```bash
marlowe-charli3 30d7c4da385a1f5044261d27b6a22d46b645ca3567636df5edeb303d \
                "OracleFeed" \
                "Charli3 ADAUSD" \
                charli3-mainnet.plutus
```


## Example

See the example of running the oracle on `mainnet`, [charli3.ipynb](charli3.ipynb), which is best viewed [here](https://nbviewer.org/github/input-output-hk/marlowe-plutus/blob/main/marlowe-plutus/charli3.ipynb), or its [video demonstration](https://youtu.be/_9DgXb323CE). Note that the example is for a slightly outdated version of this Plutus script.


## Help

```console
$ marlowe-charli3 --help

marlowe-charli3 : run a Charli3 oracle bridge for Marlowe contracts

Usage: marlowe-charli3 CURRENCY_SYMBOL TOKEN_NAME CHOICE_NAME FILE_NAME

  This command-line tool outputs a Plutus script that bridges a Charli3 oracle
  to a Marlowe contract.

Available options:
  -h,--help                Show this help text
  CURRENCY_SYMBOL          The currency symbol for the Charli3 oracle reference
                           input token. For example,
                           "30d7c4da385a1f5044261d27b6a22d46b645ca3567636df5edeb303d"
                           on mainnet and
                           "e4c846f0f87a7b4524d8e7810ed957c6b7f6e4e2e2e42d75ffe7b373"
                           on preprod.
  TOKEN_NAME               The token name for the Charli3 oracle reference
                           input token. For example, "OracleFeed".
  CHOICE_NAME              The Marlowe choice name for the oracle input to the
                           contract. For example, "Charli3 ADAUSD".
  FILE_NAME                The name of the file for the Plutus script.
```
