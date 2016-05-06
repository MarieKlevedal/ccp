all: Main

Main: Main.hs TypeChecker.hs ReturnChecker.hs AlphaRen.hs CodeGen.hs CGEnv.hs LLVM.hs ParJavalette.hs
	ghc Main.hs

ParJavalette.hs: ParJavalette.y
	alex -g LexJavalette.x
	happy -gca ParJavalette.y

ParJavalette.y: Javalette.cf
	bnfc Javalette.cf

clean:
	-rm -f *.hi *.o *.bak

distclean: clean
	-rm -f DocJavalette.* LexJavalette.* ParJavalette.* LayoutJavalette.* SkelJavalette.* PrintJavalette.* TestJavalette.* AbsJavalette.* TestJavalette ErrM.* SharedString.* ComposOp.* Javalette.dtd XMLJavalette.*
