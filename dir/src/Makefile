all: checkers

checkers: TypeChecker.o ReturnChecker.o AlphaRen.o CodeGen.o CGEnv.o LLVM.o Main.o

TypeChecker.o: TypeChecker.hs #ParJavalette.o
	ghc TypeChecker.hs

ReturnChecker.o: ReturnChecker.hs #ParJavalette.o
	ghc ReturnChecker.hs

AlphaRen.o: AlphaRen.hs #ParJavalette.o
	ghc AlphaRen.hs

CodeGen.o: CodeGen.hs #ParJavalette.o
	ghc CodeGen.hs

CGEnv.o: CGEnv.hs #ParJavalette.o
	ghc CGEnv.hs

LLVM.o: LLVM.hs #ParJavalette.o
	ghc LLVM.hs

Main.o: Main.hs TypeChecker.o ReturnChecker.o AlphaRen.o CodeGen.o CGEnv.o LLVM.o
	ghc Main.hs

ParJavalette.o: LexJavalette.x ParJavalette.y
	alex -g LexJavalette.x
	happy -gca ParJavalette.y
	ghc TestJavalette.hs -o TestJavalette

LexJavalette.x: Javalette.cf
	bnfc Javalette.cf

clean:
	-rm -f *.hi *.o *.bak

distclean: clean
	-rm -f DocJavalette.* LexJavalette.* ParJavalette.* LayoutJavalette.* SkelJavalette.* PrintJavalette.* TestJavalette.* AbsJavalette.* TestJavalette ErrM.* SharedString.* ComposOp.* Javalette.dtd XMLJavalette.*
