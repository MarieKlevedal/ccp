{-
import System.IO
import System.FilePath.Posix
import System.Environment
import System.Exit
import System.Process
-}
import Control.Monad.State

import AbsJavalette
import LLVM
import CGEnv

--------------------------- ************** ---------------------------
--------------------------- CODE GENERATOR ---------------------------
--------------------------- ************** ---------------------------

-- codeGen concatenates the boilerplate code with the code generated by the
-- compilation of the program, and returns it as a string
codeGen :: FilePath -> Program -> String
codeGen filename prg = unlines (Prelude.map instrToStr jcode)
    where
        jcode  = reverse $ code $ compileProg prg `execState` (emptyEnv filename)


-- emit takes an instruction and adds it at beginning of the code in the env
emit :: Instruction -> State Env ()
emit i = modify (\env -> env{code = i: code env})

-- emitComment emits a jasmin comment
emitComment :: String -> State Env ()
emitComment str = emit $ Comment str

-- emitBlank emits a blank row
emitBlank :: State Env ()
emitBlank = emitText ""

-- emitText emits a string
emitText :: String -> State Env ()
emitText str = emit $ Text str




--------------------------- ***************** -------------------------
--------------------------- COMPILE FUNCTIONS -------------------------
--------------------------- ***************** -------------------------

-- compileProg compiles the program
compileProg :: Program -> State Env ()
compileProg _ = undefined {-do
    addDefs defs
    compileDefs defs -}

-- compileDefs takes a list of defs and compiles all of them
compileDefs :: [Def] -> State Env ()
compileDefs []     = return ()
compileDefs (d:ds) = do
    compileDef d
    compileDefs ds

-- compileDef compiles the def
compileDef :: Def -> State Env ()
compileDef _ = undefined
    
 {-
-- compileStms compiles a list of statements 
compileStms :: [Stm] -> State Env ()
compileStms []     = return ()
compileStms (s:ss) = do
    compileStm s
    compileStms ss

-- compileStm compiles a single statement
compileStm :: Stm -> State Env ()
compileStm stm = case stm of
    SExp e        -> compileExp e
    SDecls _ ids  -> extendVars ids
    SInit  _ id e -> do
        extendVar id
        compileExp (EAss (EId id) e)
    SReturn e     -> do
        compileExp e
        emitComment "*** just about to return an int ***"
        emit IReturn
    SWhile e stm  -> do
        emitComment "*** start of while ***"
        lTest <- newLabel
        lEnd  <- newLabel
        emit $ Label lTest
        compileExp e
        emit $ IfEQ lEnd
        compileStm stm
        emit $ Goto lTest
        emit $ Label lEnd
    SBlock stms   -> do
        emitComment "*** new block ***"
        newBlock
        compileStms stms
        exitBlock
    SIfElse exp stm1 stm2 -> do
        emitComment "*** start of if-else ***"
        lTrue  <- newLabel
        lFalse <- newLabel
        compileExp exp
        emit $ IfEQ lFalse
        compileStm stm1
        emit $ Goto lTrue
        emit $ Label lFalse
        compileStm stm2
        emit $ Label lTrue

-- extendVars takes a list of Ids and adds them to the environment
extendVars :: [Id] -> State Env ()
extendVars []       = return ()
extendVars (id:ids) = do
    extendVar id
    extendVars ids

-- compileExps compiles all expressions
compileExps :: [Exp] -> State Env ()
compileExps []       = return ()
compileExps (e:exps) = do
    compileExp e
    compileExps exps

-- compileExp compiles an expression with pattern matching
compileExp :: Exp -> State Env ()
compileExp exp = case exp of
    ETrue        -> emit $ Iconst1  
    EFalse       -> emit $ Iconst0
    EInt n       -> emit $ Bipush (fromInteger n)
    EId id       -> do
        addr <- lookupVar id
        emit $ ILoad addr
    
    EApp id@(Id name) es -> do
        compileExps es
        (ts, rt) <- lookupFun id
        env <- get
        let c = if (elem name ["printInt", "readInt"])
            then "Runtime"
            else filename env
        emitComment $ "*** calling function " ++ name ++ " ***"
        emit $ Invoke c name ts rt
        
    EPostIncr e@(EId id)  -> do
        compileExp e
        addr <- lookupVar id
        emit $ Duplicate
        emit $ Iconst1
        emit $ IAdd
        emit $ IStore addr
    EPostDecr e@(EId id)  -> do
        compileExp e
        addr <- lookupVar id
        emit $ Duplicate
        emit $ Iconst1
        emit $ ISub
        emit $ IStore addr
    EPreIncr e@(EId id)  -> do
        compileExp e
        addr <- lookupVar id
        emit $ Iconst1
        emit $ IAdd
        emit $ Duplicate
        emit $ IStore addr
    EPreDecr e@(EId id)  -> do
        compileExp e
        addr <- lookupVar id
        emit $ Iconst1
        emit $ ISub
        emit $ Duplicate
        emit $ IStore addr
        
    ETimes e1 e2 -> do
        compileExp e1
        compileExp e2
        emit $ IMul
    EDiv e1 e2   -> do
        compileExp e1
        compileExp e2
        emit $ IDiv
    EPlus e1 e2  -> do
        compileExp e1
        compileExp e2
        emit $ IAdd
    EMinus e1 e2 -> do
        compileExp e1
        compileExp e2
        emit $ ISub
        
    ELt e1 e2    -> compareBool "IfCmLT" e1 e2 
    EGt e1 e2    -> compileExp (ELt e2 e1)
    ELtEq e1 e2  -> compareBool "IfCmLTEQ" e1 e2
    EGtEq e1 e2  -> compileExp (ELtEq e2 e1)
    EEq e1 e2    -> compareBool "IfCmEQ" e1 e2
    ENEq e1 e2   -> compareBool "IfCmNEQ" e1 e2 
        
    EAnd e1 e2   -> do
        compileExp e1
        lFalse <- newLabel
        lTrue <- newLabel
        emit $ IfEQ lFalse   
        compileExp e2
        emit $ Goto lTrue
        emit $ Label lFalse
        emit $ Iconst0
        emit $ Label lTrue
    EOr e1 e2    -> do
        compileExp e1
        lFalse <- newLabel
        lTrue <- newLabel
        emit $ IfEQ lFalse   
        emit $ Iconst1
        emit $ Goto lTrue
        emit $ Label lFalse
        compileExp e2
        emit $ Label lTrue
        
    EAss (EId id) e2   -> do
        compileExp e2         
        addr <- lookupVar id  
        emit $ IStore addr   
        emit $ ILoad addr     

-- compileBool compiles compare expressions
compareBool :: String -> Exp -> Exp -> State Env ()
compareBool instr e1 e2 = do
    emit $ Iconst1
    compileExp e1
    compileExp e2
    lTrue <- newLabel
    case instr of
        "IfCmLT"    -> emit $ IfCmLT lTrue
        "IfCmLTEQ"  -> emit $ IfCmLTEQ lTrue
        "IfCmEQ"    -> emit $ IfCmEQ lTrue
        "IfCmNEQ"   -> emit $ IfCmNEQ lTrue
    emit Pop
    emit Iconst0
    emit $ Label lTrue
    -}
