module CodeGen where

import Control.Monad.State

import PrintJavalette

import AbsJavalette
import LLVM
import CGEnv

--------------------------- ************** ---------------------------
--------------------------- CODE GENERATOR ---------------------------
--------------------------- ************** ---------------------------

-- codeGen concatenates the boilerplate code with the code generated by the
-- compilation of the program, and returns it as a string
codeGen :: {-FilePath ->-} Program -> String
codeGen prg = unlines (Prelude.map show (reverse (llvmCode ++ headerCode))) where
    env        = compileProg prg `execState` startEnv
    headerCode = header $ env
    llvmCode   = code $ env

-- emit takes an instruction and adds it at beginning of the code in the env
emit :: Instruction -> State Env ()
emit i = modify (\env -> env{code = i: code env})

emitHeader :: Instruction -> State Env ()
emitHeader i = modify (\env -> env{header = i: header env})

-- emitBlank emits a blank row
emitBlank :: State Env ()
emitBlank = emitText ""

-- emitText emits a string
emitText :: String -> State Env ()
emitText str = emit $ Text str
    

convertFuncArgs :: [Arg] -> State Env String
convertFuncArgs []               = return ")"
convertFuncArgs [(DArg t id)]    = do
    (Ident llvmId) <- extendVar id
    return $ (toLType t) ++ " " ++ llvmId ++ ")"
convertFuncArgs ((DArg t id):as) = do
    (Ident llvmId) <- extendVar id
    remainder      <- convertFuncArgs as
    return $ (toLType t) ++ " " ++ llvmId ++ " , " ++ remainder


emitFuncArgs :: [Arg] -> State Env ()
emitFuncArgs []                         = return ()
emitFuncArgs ((DArg t id@(Ident jId)):as) = do
    emit $ Alloca t jId
    (Ident lId) <- lookupVar id
    emit $ Store t lId jId
    
    emitFuncArgs as

--------------------------- ***************** -------------------------
--------------------------- COMPILE FUNCTIONS -------------------------
--------------------------- ***************** -------------------------

-- compileProg compiles the program
compileProg :: Program -> State Env ()
compileProg (PProg ds) = do
    addDefs ds
    emitHeader $ Text "declare void @printString(i8*)"
    emitHeader $ Text "declare void @printInt(i32)"
    emitHeader $ Text "declare void @printDouble(double)"
    emitHeader $ Text "declare i32 @readInt()"
    emitHeader $ Text "declare double @readDouble()"
    emitHeader $ Text ""
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
    emitText $ "define " ++ (toLType rt) ++ " @" ++ str ++ "(" ++ argStr ++ " {"
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
        _                   -> compileStms ss


-- compileStm compiles a single statement
compileStm :: Stm -> State Env ()
compileStm  SEmpty              = return ()

compileStm (SBlock (DBlock ss)) = compileStms ss

compileStm (SDecl t items)      = compileItems t items

compileStm (SAss (Ident str) e@(EType t _)) = do
    res <- compileExp e
    emit $ Store t res str

compileStm (SIncr jId)          = compileStm $ SAss jId $ EType TInt $ 
                                    EAdd (EType TInt (EVar jId)) Plus (ELit (LInt 1)) 

compileStm (SDecr jId)          = compileStm $ SAss jId $ EType TInt $ 
                                    EAdd (EType TInt (EVar jId)) Minus (ELit (LInt 1)) 

compileStm (SRet e@(EType t _)) = do
    ret <- compileExp e
    emit $ Return t ret

compileStm  SVRet               = emit VReturn

compileStm (SIf e s)            = do
    lTrue  <- newLabel
    lEnd   <- newLabel
    str    <- compileExp e
    emit $ Br2 str lTrue lEnd
    emit $ Label lTrue
    compileStm s
    emit $ Br1 lEnd
    emit $ Label lEnd

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
compileItems _ []                  = return ()
compileItems t ((IDecl id@(Ident s)):is)     = do
    extendVar id
    emit $ Alloca t s
    defaultValue t s
    compileItems t is
compileItems t ((IInit id@(Ident s) exp):is) = do
    extendVar id
    emit $ Alloca t s
    compileStm $ SAss id exp
    compileItems t is

-- defaultValue is an auxiliary function to compileItems. It gives the given
-- Javalette variable a default value and stores it.
defaultValue :: Type -> String -> State Env ()
defaultValue t jName = case t of
    TInt  -> emit $ Store TInt  "0"     jName
    TDoub -> emit $ Store TDoub "0.0"   jName
    TBool -> emit $ Store TBool "false" jName

----------------------------------------------------------------------------

-- compileExps compiles all expressions
compileExps :: [Exp] -> State Env [String]
compileExps []     = return []
compileExps (e:es) = do
    s1 <- compileExp e
    ss <- compileExps es
    return (s1:ss)

-- compileExp compiles an expression with pattern matching
compileExp :: Exp -> State Env String
compileExp (EType t (EVar id@(Ident jId))) = do
    --(Ident lId) <- lookupVar id
    (Ident lId) <- extendVar id
    emit $ Load lId t jId
    --extendVar id
    return lId
    
compileExp (ELit l) = case l of
    LInt n  -> return $ show n
    LDoub x -> return $ show x
    LTrue   -> return "true"
    LFalse  -> return "false"

compileExp (EApp id@(Ident name) es) = case name of
    "printString" -> do
        (Ident gVar) <- newGlobVar
        let (EType _ (ELit (LStr str))) = head es         -- must be singleton
        let len = (length str) + 1
        emitHeader $ GlobStr gVar len str
        
        (Ident lVar) <- newVar
        emit $ GetElemPtr lVar len gVar
        
        emitText $ "call void @printString(i8* " ++ lVar ++ ")"
        
        return ""

    _             -> do
        (TFun rt ats) <- lookupFun id
        argExps <- compileExps es
        let args = zip ats argExps
        case rt of
            TVoid -> do
                emit $ VFuncCall TVoid ('@':name) args
                return ""
            _     -> do
                (Ident ret) <- newVar
                emit $ FuncCall ret rt ('@':name) args
                return ret
            
compileExp (EType t (ENeg e)) = case t of
    TInt    -> compileExp $ EType t $ EAdd (ELit $ LInt 0) Minus e
    TDoub   -> compileExp $ EType t $ EAdd (ELit $ LDoub 0.0) Minus e

compileExp (ENot e) = do  
    str          <- compileExp e
    (Ident res)  <- newVar
    emit $ Xor res str "true"
    (Ident res') <- newVar
    emit $ Xor res' res "false"
    return res'


compileExp (EType t (EMul e1 op e2)) = do
    str1        <- compileExp e1
    str2        <- compileExp e2
    (Ident res) <- newVar
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
    (Ident res) <- newVar
    case (op, t) of
        (Plus, TInt)    -> emit $ IAdd res str1 str2
        (Plus, TDoub)   -> emit $ FAdd res str1 str2
        (Minus, TInt)   -> emit $ ISub res str1 str2
        (Minus, TDoub)  -> emit $ FSub res str1 str2
    return res 

compileExp (ERel e1@(EType t _) op e2) = do
    str1        <- compileExp e1
    str2        <- compileExp e2
    (Ident res) <- newVar
    case t of
        TInt    -> emit $ ICmp res op str1 str2
        TDoub   -> emit $ FCmp res op str1 str2
        TBool   -> emit $ BCmp res op str1 str2
    return res

compileExp (EAnd e1 e2) = do
    str1        <- compileExp e1
    str2        <- compileExp e2
    (Ident res) <- newVar
    emit $ And res str1 str2
    return res

compileExp (EOr e1 e2) = do
    str1        <- compileExp e1
    str2        <- compileExp e2
    (Ident res) <- newVar
    emit $ Or res str1 str2
    return res

compileExp (EType t e) = compileExp e 

