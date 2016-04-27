all: compiler TypeChecker.o  ReturnChecker.o Main.o

TypeChecker.o: TypeChecker.hs
	ghc TypeChecker.hs

ReturnChecker.o: ReturnChecker.hs
	ghc ReturnChecker.hs

Main.o: Main.hs
	ghc Main.hs

compiler: Javalette.cf
	make distclean
	bnfc Javalette.cf
	happy -gca ParJavalette.y
	alex -g LexJavalette.x

clean:
	-rm -f *.hi *.o

distclean: clean
	-rm -f DocJavalette.* LexJavalette.* ParJavalette.* LayoutJavalette.* SkelJavalette.* PrintJavalette.* TestJavalette.* AbsJavalette.* TestJavalette ErrM.* SharedString.* ComposOp.* Javalette.dtd XMLJavalette.*
