#!/bin/bash

cd src

# make      # uncomment when we want to test

ghc TypeChecker.hs
ghc Main.hs

./Main ../tester/testsuite/bad/bad001.jl
./Main ../tester/testsuite/bad/bad002.jl
./Main ../tester/testsuite/bad/bad003.jl
./Main ../tester/testsuite/bad/bad004.jl
./Main ../tester/testsuite/bad/bad005.jl
./Main ../tester/testsuite/bad/bad006.jl
./Main ../tester/testsuite/bad/bad007.jl
./Main ../tester/testsuite/bad/bad008.jl
./Main ../tester/testsuite/bad/bad009.jl
./Main ../tester/testsuite/bad/bad010.jl
./Main ../tester/testsuite/bad/bad011.jl
./Main ../tester/testsuite/bad/bad012.jl
./Main ../tester/testsuite/bad/bad013.jl
./Main ../tester/testsuite/bad/bad015.jl
./Main ../tester/testsuite/bad/bad016.jl
./Main ../tester/testsuite/bad/bad017.jl
./Main ../tester/testsuite/bad/bad018.jl
./Main ../tester/testsuite/bad/bad019.jl
./Main ../tester/testsuite/bad/bad020.jl
