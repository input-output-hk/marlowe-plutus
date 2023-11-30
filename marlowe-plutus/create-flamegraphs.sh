#!/usr/bin/env nix-shell
#! nix-shell -i bash -p flamegraph
# shellcheck shell=bash

set -eo pipefail

UPLC="$(nix build 'github:input-output-hk/plutus?ref=1.17.0.0#uplc' --no-link --print-out-paths)/bin/uplc"
TRACETOSTACKS="$(nix build 'github:input-output-hk/plutus?ref=1.17.0.0#traceToStacks' --no-link --print-out-paths)/bin/traceToStacks"

for f in out/semantics/*-uplc.flat
do

  t=$(basename "${f%%-uplc.flat}")
  echo "Tx $t"
  l="${f%%.flat}.log"
  s="${f%%.flat}.steps.svg"
  m="${f%%.flat}.memory.svg"

  "$UPLC" evaluate \
      --input "$f" \
      --input-format flat-namedDeBruijn \
      --trace-mode LogsWithBudgets \
      --output "$l"

  "$TRACETOSTACKS" --file "$l" --column 1 \
  | flamegraph.pl \
  > "$s"

  sed -e '/^<text id="title"/s/>.*</>Steps in Tx '"$t"'</' \
      -e '3s/width="[^"]*"/width="100%"/' \
      -e '3s/height="[^"]*"/height="100%"/' \
      -i "$s"

  "$TRACETOSTACKS" --file "$l" --column 2 \
  | flamegraph.pl \
  > "$m"

  sed -e '/^<text id="title"/s/>.*</>Memory in Tx '"$t"'</' \
      -e '3s/width="[^"]*"/width="100%"/' \
      -e '3s/height="[^"]*"/height="100%"/' \
      -i "$m"

done
