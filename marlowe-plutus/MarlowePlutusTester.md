# Manually Testing a Marlowe Validator

The executable `marlowe-plutus-tester` runs the tests for conformance of an externally-supplied pair of Marlowe validators to the [Marlowe-Cardano Specification](https://github.com/input-output-hk/marlowe-cardano/blob/main/marlowe/specification/ReadMe.md).


## Usage

The `marlowe-plutus-tester` requires at least two arguments:

- The serialized Cardano text-envelope file for a Plutus V2 Marlowe semantics validator.
- The serialized Cardano text-envelope file for a Plutus V2 Marlowe role-payout validator.
- Any `hspec` flags to modify which tests are run.


## Example

```console
$ cabal run marlowe-plutus-tester -- marlowe-semantics.plutus marlowe-rolepayout.plutus

Marlowe validator
  Should be a reasonable size [✔]
  Valid transactions
    Noiseless [✔]
      +++ OK, passed 100 tests.
    Noisy [✔]
      +++ OK, passed 100 tests.
  Constraint 2. Single Marlowe script input [✔]
    +++ OK, passed 100 tests.
  Constraint 3. Single Marlowe output [✔]
    +++ OK, passed 100 tests.
  Constraint 4. No output to script on close [✔]
    +++ OK, passed 100 tests.
  Constraint 5. Input value from script [✔]
    +++ OK, passed 100 tests.
  Constraint 6. Output value to script [✔]
    +++ OK, passed 100 tests.
  Constraint 9. Marlowe parameters [✔]
    +++ OK, passed 100 tests.
  Constraint 10. Output state [✔]
    +++ OK, passed 100 tests.
  Constraint 11. Output contract [✔]
    +++ OK, passed 100 tests.
  Constraint 12. Merkleized continuations
    Valid merkleization [✔]
      +++ OK, passed 100 tests.
    Invalid merkleization [✔]
      +++ OK, passed 100 tests.
  Constraint 13. Positive balances [✔]
    +++ OK, passed 100 tests.
  Constraint 14. Inputs authorized [✔]
    +++ OK, passed 100 tests.
  Constraint 15. Sufficient payment [✔]
    +++ OK, passed 100 tests.
  Constraint 18. Final balance [✔]
    +++ OK, passed 100 tests.
  Constraint 19. No duplicates [✔]
    +++ OK, passed 100 tests.
  Constraint 20. Single satisfaction [✔]
    +++ OK, passed 100 tests.
  Hash golden test [✔]
    +++ OK, passed 1 test.
Payout validator
  Valid transactions
    Noiseless [✔]
      +++ OK, passed 100 tests.
    Noisy [✔]
      +++ OK, passed 100 tests.
  Constraint 17. Payment authorized
    Invalid authorization for withdrawal [✔]
      +++ OK, passed 100 tests.
    Missing authorized withdrawal [✔]
      +++ OK, passed 100 tests.
  Hash golden test [✔]
    +++ OK, passed 1 test.

Finished in 56.4699 seconds
25 examples, 0 failures
```
