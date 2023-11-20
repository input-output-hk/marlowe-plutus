#!/usr/bin/env nix-shell
#!nix-shell -i bash -p "rWrapper.override{packages = [ rPackages.data_table rPackages.magrittr rPackages.ggplot2 ];}" -p graphviz
# shellcheck shell=bash

echo "Run this script from its folder."

for r in r R
do
  for p in p P
  do
    for a in a A
    do
      for c in c C
      do
        for b in b B
        do
          if [[ "$r" == "r" ]]
          then
            FLAGS="+check-preconditions"
          else
            FLAGS="-check-preconditions"
          fi
          if [[ "$p" == "p" ]]
          then
            FLAGS="$FLAGS +check-positive-balances"
          else
            FLAGS="$FLAGS -check-positive-balances"
          fi
          if [[ "$a" == "a" ]]
          then
            FLAGS="$FLAGS +check-duplicate-accounts"
          else
            FLAGS="$FLAGS -check-duplicate-accounts"
          fi
          if [[ "$c" == "c" ]]
          then
            FLAGS="$FLAGS +check-duplicate-choices"
          else
            FLAGS="$FLAGS -check-duplicate-choices"
          fi
  	  if [[ "$b" == "b" ]]
  	  then
  	    FLAGS="$FLAGS +check-duplicate-bindings"
  	  else
  	    FLAGS="$FLAGS -check-duplicate-bindings"
  	  fi
  	  LABEL="$r$p$a$c$b"
	  if [[ -e "marlowe-semantics-$LABEL.tsv" ]]
	  then 
	    echo "LABEL=$LABEL already computed."
	  else
  	    set -e
  	    echo
  	    echo "FLAGS=$FLAGS"
  	    echo
	    cabal build .. --flags="$FLAGS"
  	    set +e
  	    echo
  	    echo "LABEL=$LABEL"
  	    echo
  	    cabal test marlowe-plutus --flags="$FLAGS" >& "$LABEL.log"
  	    cabal run marlowe-validators --flags="$FLAGS" && mv out/marlowe-semantics.tsv "marlowe-semantics-$LABEL.tsv"
	  fi
        done
      done
    done
  done
done

echo "Writing results:"
echo "- full.tsv = raw dataset"
echo "- summary.tsv = statistical summary (absolute)"
echo "- relative.tsv = statistical summary (relative)"
echo "- Memory.svg = Galois lattice of Plutus memory results (SVG)"
echo "- Memory.png = Galois lattice of Plutus memory results (PNG)"
echo "- Steps.svg = Galois lattice of Plutus steps results (SVG)"
echo "- Steps.png = Galois lattice of Plutus steps results (PNG)"

R CMD BATCH measure.R

dot -Tsvg -o Memory.svg Memory.dot
dot -Tpng -o Memory.png Memory.dot
dot -Tsvg -o Steps.svg Memory.dot
dot -Tpng -o Steps.png Memory.dot
