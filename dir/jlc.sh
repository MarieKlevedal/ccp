#!/bin/bash

cd src

bnfc -m -haskell Javalette.cf
make

ghc TypeChecker.hs
ghc Main.hs

# do more things?

make clean          # to remove files from previous run
make distclean      # to remove files from previous run
