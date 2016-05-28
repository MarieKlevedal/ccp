module CodeGen where

import Control.Monad.State

import PrintJavalette

import AbsJavalette
import LLVM
import CGEnv

--------------------------- ************** ---------------------------
--------------------------- CODE GENERATOR ---------------------------
--------------------------- ************** ---------------------------

-- codeGen generates the LLVM code of a program and returns it as a string
codeGen :: Program -> String
codeGen prg = unlines (headerCode ++ llvmCode) where
    env        = compileProg prg `execState` startEnv
    headerCode = Prelude.map show (reverse $ header env)
    llvmCode   = Prelude.map show (reverse $ code env)

-- emit takes an instruction and adds it at beginning of the code in the env
emit :: Instruction -> State Env ()
emit i = modify (\env -> env{code = i: code env})

-- emitHeader takes a header instruction and adds it at the beginning of the
-- header code in the env
emitHeader :: HeaderInstr -> State Env ()
emitHeader i = modify (\env -> env{header = i: header env})

-- emitBlank emits a blank row
emitBlank :: State Env ()
emitBlank = emitText ""

-- emitText emits a string
emitText :: String -> State Env ()
emitText str = emit $ Text str
    
-- convertFuncArgs creates a string representing the arguments to a function in
-- a "define" command
convertFuncArgs :: [Arg] -> State Env String
convertFuncArgs []               = return ")"
convertFuncArgs [(DArg t id)]    = do
    (Ident llvmId) <- extendVar id "t"
    return $ (showType t) ++ " " ++ llvmId ++ ")"
convertFuncArgs ((DArg t id):as) = do
    (Ident llvmId) <- extendVar id "t"
    remainder      <- convertFuncArgs as
    return $ (showType t) ++ " " ++ llvmId ++ " , " ++ remainder

-- emitFuncArgs allocates memory for the variables given to a function and creates
-- corresponding LLVM variables
emitFuncArgs :: [Arg] -> State Env ()
emitFuncArgs []                           = return ()
emitFuncArgs ((DArg t id@(Ident jId)):as) = do
    emit $ Alloca t jId
    (Ident lId) <- lookupVar id
    emit $ Store t lId jId
    emitFuncArgs as

--------------------------- ***************** -------------------------
--------------------------- COMPILE FUNCTIONS -------------------------
--------------------------- ***************** -------------------------

-- compileProg adds the functions of the program to the env, emits some static
-- header code and compiles the program
compileProg :: Program -> State Env ()
compileProg (PProg ds) = do
    addDefs ds
    emitHeader $ FuncDecl TVoid "@printString" [TStr]
    emitHeader $ FuncDecl TVoid "@printInt"    [TInt]
    emitHeader $ FuncDecl TVoid "@printDouble" [TDoub]
    emitHeader $ FuncDecl TInt  "@readInt"     []
    emitHeader $ FuncDecl TDoub "@readDouble"  []
    emitHeader $ FuncDecl TStr  "@calloc"      [TInt, TInt]
    emitHeader Empty
    emitHeader $ CreateArr "%intArr"  "%intSArr"  TInt  
    emitHeader $ CreateArr "%doubArr" "%doubSArr" TDoub
    emitHeader $ CreateArr "%boolArr" "%boolSArr" TBool
    compileDefs ds 

-- compileDefs takes a list of defs and compiles all of them
compileDefs :: [Def] -> State Env ()
compileDefs []     = return ()
compileDefs (d:ds) = do
    compileDef d
    compileDefs ds

-- compileDef compiles a definition (i.e. function)
compileDef :: Def -> State Env ()
compileDef (FnDef rt id@(Ident str) args (DBlock ss)) = do
    argStr <- convertFuncArgs args

    emitBlank
    emitText $ "define " ++ (showType rt) ++ " @" ++ str ++ "(" ++ argStr ++ " {"
    emit $ Label "entry"
    emitFuncArgs args
    
    case length ss of
        0   -> emit VReturn
        _   -> compileStms ss
    
    emitBlank
    emitText "}"   
 
-- compileStms compiles a list of statements 
compileStms :: [Stm] -> State Env ()
compileStms []     = return ()
compileStms (s:ss) = do
    compileStm s
    case (ss, s) of
        ([], SIf     _ _  )                       -> emitText "unreachable"
        ([], SIfElse _ _ _)                       -> emitText "unreachable"
        ([], SWhile  _ _  )                       -> emitText "unreachable"
        (_ , SWhile (EType TBool (ELit LTrue)) _) -> emitText "unreachable"
        _                                         -> compileStms ss

--------------------------------------------------------------------------------
----------------- compileStm compiles a single statement -----------------------
--------------------------------------------------------------------------------
compileStm :: Stm -> State Env ()
compileStm  SEmpty              = return ()

compileStm (SBlock (DBlock ss)) = compileBlock ss where 
    compileBlock []     = return ()
    compileBlock (s:ss) = do
        compileStm s
        case s of
            SWhile (EType TBool (ELit LTrue)) _ -> emitText "unreachable"
            _                                   -> compileBlock ss

compileStm (SDecl t items)      = compileItems t items

compileStm (SAss (Ident str) e@(EType t _)) = do
    res <- compileExp e
    emit $ Store t res str

compileStm (SArrAss id e1 te2@(EType t e2))   = do -- id[e1] = e2
    elem         <- compileExp te2
    idx          <- compileExp e1
    (Ident lArr) <- lookupVar id
    (Ident lIdx) <- newLocVar "idx" 
    emit $ GEP_Index lIdx t lArr idx
    --(Ident ret)  <- newLocVar "t"
    emit $ Store t elem lIdx
    return ()

compileStm (SNewArrAss id t e)  = do -- id = new t[e]
    -- calculate the size of t
    (Ident t1)  <- newLocVar "t"
    (Ident t2)  <- newLocVar "t"
    emit $ GEP_Size t1 t t2
    
    -- allocate memory on heap for the array and init elems to 0
    len         <- compileExp e
    (Ident t3)  <- newLocVar "t"
    emit $ FuncCall t3 TStr "@calloc" [(TInt, len), (TInt, t2)]
    
    -- bitcast pointer to allocated memory on heap to the array type for t
    (Ident lArr) <- extendVar id "arr"
    emit $ CallBitCast lArr t3 t

    -- set length of array
    (Ident lLen) <- newLocVar "len"
    emit $ GEP_Length lLen t lArr
    emit $ Store TInt len lLen

    return ()

compileStm (SIncr jId)          = compileStm $ SAss jId $ EType TInt $ 
                                    EAdd (EType TInt (EVar jId)) Plus (ELit (LInt 1)) 

compileStm (SDecr jId)          = compileStm $ SAss jId $ EType TInt $ 
                                    EAdd (EType TInt (EVar jId)) Minus (ELit (LInt 1)) 

compileStm (SRet e@(EType t _)) = do
    ret <- compileExp e
    emit $ Return t ret

compileStm  SVRet               = emit VReturn

compileStm (SIf e s)            = compileStm $ SIfElse e s SEmpty

compileStm (SIfElse e s1 s2)    = do
    lTrue  <- newLabel
    lFalse <- newLabel
    lEnd   <- newLabel
    str    <- compileExp e
    emit $ Br2 str lTrue lFalse
    emit $ Label lTrue
    compileStm s1
    emit $ Br1 lEnd
    emit $ Label lFalse
    compileStm s2
    emit $ Br1 lEnd
    emit $ Label lEnd
        
compileStm (SWhile e s)         = do
    lTestPred <- newLabel
    lTrue     <- newLabel
    lEnd      <- newLabel
    emit $ Br1 lTestPred
    emit $ Label lTestPred
    str       <- compileExp e
    emit $ Br2 str lTrue lEnd
    emit $ Label lTrue
    compileStm s
    emit $ Br1 lTestPred
    emit $ Label lEnd

compileStm (SExp e)             = do
    compileExp e
    return ()

-- compileItems is an auxiliary function to compileStm. It takes a list of
-- variables to be allocated. It alllocates them and gives them init values
-- (default values for the ones that don't have a user defined init value).
compileItems :: Type -> [Item] -> State Env ()
compileItems _ []                                  = return ()
compileItems at@(TArr t) ((IDecl id@(Ident s)):is) = compileItems at is
compileItems t ((IDecl id@(Ident s)):is)           = do
    extendVar id "t"
    emit $ Alloca t s
    defaultValue t s
    compileItems t is
compileItems t ((IInit id@(Ident s) exp):is)       = do
    extendVar id "t"
    emit $ Alloca t s
    compileStm $ SAss id exp
    compileItems t is
compileItems at ((IArrInit id@(Ident s) t exp):is) = do
    compileStm $ SNewArrAss id t exp
    compileItems at is

-- defaultValue is an auxiliary function to compileItems. It gives the given
-- Javalette variable a default value and stores it.
defaultValue :: Type -> String -> State Env ()
defaultValue t jName = case t of
    TInt  -> emit $ Store TInt  "0"     jName
    TDoub -> emit $ Store TDoub "0.0"   jName
    TBool -> emit $ Store TBool "false" jName


--------------------------------------------------------------------------------
----------------- compileExp compiles an expression ----------------------------
--------------------------------------------------------------------------------
compileExp :: Exp -> State Env String
compileExp (EType t (EVar id@(Ident jId))) = do
    (Ident lId) <- extendVar id "t"
    emit $ Load lId t jId
    return lId
    
compileExp (ELit l) = case l of
    LInt  n -> return $ show n
    LDoub x -> return $ show x
    LTrue   -> return "true"
    LFalse  -> return "false"

compileExp (EApp id@(Ident name) es) = case name of
    "printString" -> do
        (Ident gVar) <- newGlobVar
        let (EType _ (ELit (LStr str))) = head es       -- must be singleton
        let len = (length str) + 1
        emitHeader $ GlobStr gVar len str
        
        (Ident lVar) <- newLocVar "t"
        emit $ GEP_String lVar len gVar
        
        emitText $ "call void @printString(i8* " ++ lVar ++ ")"
        
        return ""

    _             -> do
        (TFun rt ats) <- lookupFun id
        argExps <- compileArgExps es
        let args = zip ats argExps
        case rt of
            TVoid -> do
                emit $ VFuncCall TVoid ('@':name) args
                return ""
            _     -> do
                (Ident ret) <- newLocVar "t"
                emit $ FuncCall ret rt ('@':name) args
                return ret
            
compileExp (EType t (EArrLen id)) = do    -- id.length
    (Ident lArr) <- lookupVar id
    (Ident len)  <- newLocVar "len"
    emit $ GEP_Length len t lArr
    (Ident res)  <- newLocVar "t"
    emit $ Load res TInt len
    return res
  
compileExp (EType t (EArrInd id e)) = do    -- id[e]
    idx          <- compileExp e
    (Ident lArr) <- lookupVar id
    (Ident lIdx) <- newLocVar "idx" 
    emit $ GEP_Index lIdx t lArr idx
    (Ident ret)  <- newLocVar "t"
    emit $ Load ret t lIdx
    return ret
            
compileExp (EType t (ENeg e)) = case t of
    TInt    -> compileExp $ EType t $ EAdd (ELit $ LInt 0) Minus e
    TDoub   -> compileExp $ EType t $ EAdd (ELit $ LDoub 0.0) Minus e

compileExp (ENot e) = do  
    str          <- compileExp e
    (Ident res)  <- newLocVar "t"
    emit $ BCmp res Eq str "false"
    return res

compileExp (EType t (EMul e1 op e2)) = do
    str1        <- compileExp e1
    str2        <- compileExp e2
    (Ident res) <- newLocVar "t"
    case (op, t) of
        (Times, TInt)   -> emit $ IMul res str1 str2
        (Times, TDoub)  -> emit $ FMul res str1 str2
        (Div  , TInt)   -> emit $ IDiv res str1 str2
        (Div  , TDoub)  -> emit $ FDiv res str1 str2
        (Mod  , TInt)   -> emit $ IMod res str1 str2
    return res

compileExp (EType t (EAdd e1 op e2)) = do
    str1        <- compileExp e1
    str2        <- compileExp e2
    (Ident res) <- newLocVar "t"
    case (op, t) of
        (Plus, TInt)    -> emit $ IAdd res str1 str2
        (Plus, TDoub)   -> emit $ FAdd res str1 str2
        (Minus, TInt)   -> emit $ ISub res str1 str2
        (Minus, TDoub)  -> emit $ FSub res str1 str2
    return res 

compileExp (ERel e1@(EType t _) op e2) = do
    str1        <- compileExp e1
    str2        <- compileExp e2
    (Ident res) <- newLocVar "t"
    case t of
        TInt    -> emit $ ICmp res op str1 str2
        TDoub   -> emit $ FCmp res op str1 str2
        TBool   -> emit $ BCmp res op str1 str2
    return res

compileExp(EAnd e1 e2) = compileAndOr "and" e1 e2

compileExp (EOr e1 e2) = compileAndOr "or" e1 e2

compileExp (EType t e) = compileExp e 

-- compileArgExps is an auxiliary function to the EApp case in compileExp. It 
-- compiles all expressions in the given list
compileArgExps :: [Exp] -> State Env [String]
compileArgExps []     = return []
compileArgExps (e:es) = do
    s1 <- compileExp e
    ss <- compileArgExps es
    return (s1:ss)

-- compileAndOr is an auxiliary funcion to the EAnd and EOr cases in compileExp
compileAndOr :: String -> Exp -> Exp -> State Env String
compileAndOr op e1 e2 = do
    lTrue           <- newLabel
    lFalse          <- newLabel
    lEnd            <- newLabel
    str1            <- compileExp e1
    rId@(Ident res) <- newLocVar "t"
    extendVar rId "t"
    emit $ Alloca TBool res
    emit $ Br2 str1 lTrue lFalse
    emit $ Label lTrue
    case op of
        "and" -> do
            str2 <- compileExp e2           
            emit $ Store TBool str2 res     
        "or"  -> emit $ Store TBool "true" res
    emit $ Br1 lEnd
    emit $ Label lFalse
    case op of
        "and" -> emit $ Store TBool "false" res
        "or"  -> do
            str2 <- compileExp e2
            emit $ Store TBool str2 res 
    emit $ Br1 lEnd
    emit $ Label lEnd
    (Ident res') <- newLocVar "t"
    emit $ Load res' TBool res
    return res'

