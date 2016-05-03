module CodeGen where

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
codeGen :: {-FilePath ->-} Program -> String
codeGen prg = unlines (Prelude.map show llvmCode) where
    llvmCode  = reverse $ code $ compileProg prg `execState` emptyEnv
{-
codeGen filename prg = unlines (Prelude.map instrToStr jcode)
    where
        jcode  = reverse $ code $ compileProg prg `execState` (emptyEnv filename)
-}

-- emit takes an instruction and adds it at beginning of the code in the env
emit :: Instruction -> State Env ()
emit i = modify (\env -> env{code = i: code env})

{-
-- emitComment emits a jasmin comment
emitComment :: String -> State Env ()
emitComment str = emit $ Comment str
-}
-- emitBlank emits a blank row
emitBlank :: State Env ()
emitBlank = emitText ""

-- emitText emits a string
emitText :: String -> State Env ()
emitText str = emit $ Text str
    

convertFuncArgs :: [Arg] -> State Env String
convertFuncArgs []               = return ")"
convertFuncArgs [(DArg t id)]    = do
    (Ident llvmId) <- allocateVar id
    return $ (toLType t) ++ " " ++ llvmId ++ ")"
convertFuncArgs ((DArg t id):as) = do
    (Ident llvmId) <- allocateVar id
    remainder      <- convertFuncArgs as
    return $ (toLType t) ++ " " ++ llvmId ++ " , " ++ remainder


emitFuncArgs :: [Arg] -> State Env ()
emitFuncArgs []                 = return ()
emitFuncArgs (a@(DArg t id@(Ident s)):as) = do
    emit $ Alloca t s
    (Ident s2) <- lookupVar id
    emit $ Store t s2 s
    emitFuncArgs as

--------------------------- ***************** -------------------------
--------------------------- COMPILE FUNCTIONS -------------------------
--------------------------- ***************** -------------------------

-- compileProg compiles the program
compileProg :: Program -> State Env ()
compileProg (PProg ds) = do
    addDefs ds
    compileDefs ds 

-- compileDefs takes a list of defs and compiles all of them
compileDefs :: [Def] -> State Env ()
compileDefs []     = return ()
compileDefs (d:ds) = do
    compileDef d
    compileDefs ds

-- compileDef compiles the def
compileDef :: Def -> State Env ()
compileDef (FnDef rt id@(Ident str) args (DBlock ss)) = do
    argStr <- convertFuncArgs args
    emitText $ "define " ++ (toLType rt) ++ " @" ++ str ++ "(" ++ argStr ++ " {"
    emit $ Label "entry"
    emitFuncArgs args
    
    compileStms ss
    
    emitText "}"   

 
-- compileStms compiles a list of statements 
compileStms :: [Stm] -> State Env ()
compileStms []     = return ()
compileStms (s:ss) = do
    compileStm s
    compileStms ss


-- compileStm compiles a single statement
compileStm :: Stm -> State Env ()
compileStm  SEmpty              = return ()
compileStm (SBlock (DBlock ss)) = compileStms ss
compileStm (SDecl t items)      = compileItems t items
compileStm (SAss (Ident str) e@(EType t _)) = do
    res <- compileExp e
    emit $ Store t res str
compileStm (SIncr id)           = error "SIncr nyi"
compileStm (SDecr id)           = error "SDecr nyi"
compileStm (SRet e@(EType t _)) = do
    ret <- compileExp e
    emit $ Return t ret
compileStm  SVRet               = error "SVRet nyi"
compileStm (SIf e s)            = error "SIf nyi"
compileStm (SIfElse e s1 s2)    = error "SIfElse nyi"    
compileStm (SWhile e s)         = error "SWhile nyi"
compileStm (SExp e)             = do
    compileExp e
    return ()

    {-
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
-}

compileItems :: Type -> [Item] -> State Env ()
compileItems _ []                  = return ()
compileItems t ((IDecl id@(Ident s)):is)     = do
    allocateVar id
    emit $ Alloca t s
    compileItems t is
compileItems t ((IInit id@(Ident s) exp):is) = do
    allocateVar id
    emit $ Alloca t s
    compileStm $ SAss id exp
    compileItems t is

{-
-- compileExps compiles all expressions
compileExps :: [Exp] -> State Env ()
compileExps []       = return ()
compileExps (e:exps) = do
    compileExp e
    compileExps exps
-}
-- compileExp compiles an expression with pattern matching
compileExp :: Exp -> State Env String
compileExp (EType t (EVar id@(Ident s1))) = do
    (Ident s2) <- lookupVar id
    emit $ Load s2 t s1
    return s2
compileExp (ELit l) = case l of
    LInt n  -> return $ show n
    LDoub x -> return $ show x
    LTrue   -> return "true"
    LFalse  -> return "false"
    LStr s  -> return s                           -- TODO remove?

compileExp (EType t (EAdd e1 Plus e2)) = do
    str1        <- compileExp e1
    str2        <- compileExp e2
    (Ident res) <- newVar
    case t of
        TInt  -> emit $ IAdd res str1 str2
        TDoub -> return ()                          --TODO
    return res 

compileExp (EType t e) = compileExp e 

compileExp e = error "exp nyi"
{-
compileExp exp = case exp of
    ETrue        -> emit $ Iconst1  
    EFalse       -> emit $ Iconst0

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
