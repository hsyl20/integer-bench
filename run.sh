#!/bin/sh
cabal new-update
rm -rf ~/.cabal/store
rm -rf dist-newstyle
cabal new-build -w ~/projects/ghc/bindists/integer-gmp/bin/ghc
cabal new-run integer-bench -w ~/projects/ghc/bindists/integer-gmp/bin/ghc > res_gmp

rm -rf ~/.cabal/store
rm -rf dist-newstyle
cabal new-build -w ~/projects/ghc/bindists/integer-simple/bin/ghc
cabal new-run integer-bench -w ~/projects/ghc/bindists/integer-simple/bin/ghc > res_simple

rm -rf ~/.cabal/store
rm -rf dist-newstyle
cabal new-build -w ~/projects/ghc/bindists/integer-bignum/bin/ghc
cabal new-run integer-bench -w ~/projects/ghc/bindists/integer-bignum/bin/ghc > res_bignum
