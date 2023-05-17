#!/bin/sh
cabal build $PWD
./dist-newstyle/build/aarch64-osx/ghc-9.2.2/haskell-sql-toy-0.1.0.0/x/haskell-sql-toy/build/haskell-sql-toy/haskell-sql-toy $PWD/example.json
