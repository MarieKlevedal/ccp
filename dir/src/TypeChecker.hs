module TypeChecker where

import AbsJavalette
import PrintJavalette
import ErrM

import Data.Map
import qualified Data.Map as M
import Control.Monad.State

----------------------------------- ************** ----------------------------------
----------------------------------- NEW DATA TYPES ----------------------------------
----------------------------------- ************** ----------------------------------

data Env = Env {
    funcs :: Map Ident Type,
    cxts  :: [Map Ident Type]
    }

type TypeCheckM a = StateT Env Err a

--------------------------------- ******************* --------------------------------
--------------------------------- AUXILIARY FUNCTIONS --------------------------------
--------------------------------- ******************* --------------------------------

-- lookVar returns the type of the variable in the topmost (i.e. innermost) context of 
-- the environment in which the variable exists.
lookVar :: Ident -> TypeCheckM Type
lookVar id = do
    env <- get
    lookVar' id (cxts env)

-- lookVar' is a helper function to lookVar.
lookVar' :: Ident -> [Map Ident Type] -> TypeCheckM Type
lookVar' id []     = fail $ "No variable with id " ++ printTree id ++ " in the environment"
lookVar' id (c:cs) = case M.lookup id c of
    Just t  -> return t
    Nothing -> lookVar' id cs

-- lookFunSig returns the function type signature for the function with matching id.
lookFuncSig :: Ident -> TypeCheckM Type
lookFuncSig id = do
    env <- get
    case M.lookup id (funcs env) of
        Just t  -> return t
        Nothing -> fail $ "No function with id " ++ printTree id ++ " in the environment"

-- extendVar adds a new variable and its type to the topmost (i.e. innermost) context 
-- in the environment
extendVar :: Ident -> Type -> TypeCheckM ()
extendVar id t = do
    env <- get
    let (c:cs) = cxts env
    case M.lookup id c of
        Just _  -> fail $ "Variable " ++ printTree id ++ " already in context"
        Nothing -> modify (\env -> env{cxts = (M.insert id t c):cs})       
    
-- extendFun adds a new function and its type to the envionment
extendFuncSig :: Ident -> Type -> TypeCheckM ()
extendFuncSig id t = do
    env <- get
    let f = funcs env
    case M.lookup id f of
        Just _  -> fail $ "Function with id " ++ printTree id ++ " already exists"
        Nothing -> modify (\env -> env{funcs = M.insert id t f})

-- newCxt adds a new, empty block/context to the environment 
newCxt :: TypeCheckM ()
newCxt = modify (\env -> env{cxts = (M.empty):(cxts env)})

-- rmCxt removes he topmost block/context from the environment
rmCxt :: TypeCheckM ()
rmCxt = modify (\env -> env{cxts = tail (cxts env)})

-- emptyEnv returns an empty environment
emptyEnv :: Env
emptyEnv = Env M.empty []

----------------------------------- ********** -----------------------------------
----------------------------------- ADD THINGS -----------------------------------
----------------------------------- ********** -----------------------------------

-- addFtoEnv adds the function type signature of each definition to the environment 
addFToEnv :: [Def] -> TypeCheckM ()
addFToEnv []                       = return ()
addFToEnv ((FnDef t id args _):ds) = do
    extendFuncSig id (TFun t (Prelude.map (\(DArg t id) -> t) args))
    addFToEnv ds

-- addVtoC adds all argument varibles to the topmost/innermost context in env
addVToC :: [Arg] -> TypeCheckM ()
addVToC []                 = return ()
addVToC ((DArg t id):args) = do
    extendVar id t
    addVToC args

---------------------------------- ************ -----------------------------------
---------------------------------- TYPECHECKING -----------------------------------
---------------------------------- ************ -----------------------------------

-- typecheck first adds all the definitions of the program along with the built in
-- functions to the environment and then makes sure all definitions are type correct.
-- It returns a type correct and type annotated program.
typecheck :: Program -> Err Program
typecheck (PProg ds) = do
    (ds', _) <- runStateT ((addFToEnv (ds ++ builtIn)) >> (checkDefs ds)) emptyEnv
    return $ PProg ds'

builtIn :: [Def]
builtIn = [(FnDef TVoid (Ident "printInt")    [DArg TInt (Ident "n")]  (DBlock [])), 
           (FnDef TVoid (Ident "printDouble") [DArg TDoub (Ident "x")] (DBlock [])),
           (FnDef TInt  (Ident "readInt")     []                       (DBlock [])),
           (FnDef TDoub (Ident "readDouble")  []                       (DBlock [])),
           (FnDef TVoid (Ident "printString") [DArg TStr (Ident "s")]  (DBlock []))]

-- checkDefs calls checkDef for all definitions in the program. It returns all the
-- type correct and type annotated function definitions.
checkDefs :: [Def] -> TypeCheckM [Def]
checkDefs []     = return [] 
checkDefs (d:ds) = do
    d'  <- checkDef d
    ds' <- checkDefs ds
    return $ (d':ds')

-- checkDef adds a new context to the env, adds all the func args to the context and 
-- checks that all stms in its body are type correct. It returns a type correct and
-- type annotated function definition.
checkDef :: Def -> TypeCheckM Def
checkDef (FnDef t id args (DBlock stms)) = do
    newCxt
    addVToC args
    stms' <- checkStms t stms
    rmCxt
    return $ FnDef t id args (DBlock stms')


----------------------- checkStms and its auxilary functions ----------------------

-- checkStms calls checkStm for all statements in one definition block. It returns
-- all the type correct and type annotated statements.
checkStms :: Type -> [Stm] -> TypeCheckM [Stm]
checkStms _ []     = return []
checkStms t (s:ss) = do
    s'  <- checkStm t s
    ss' <- checkStms t ss
    return (s':ss')

-- checkStm checks that stm is type correct. It returns the statement, with all 
-- its sub-expressions type-annotated.
checkStm :: Type -> Stm -> TypeCheckM Stm
checkStm rt stm = case stm of
    SEmpty                -> return stm
    
    SBlock (DBlock stms)  -> do
        newCxt
        stms' <- checkStms rt stms
        rmCxt
        return (SBlock (DBlock stms'))
        
    SDecl t items         -> do 
        items' <- checkDecl t items
        return (SDecl t items')
                    
    SAss id exp           -> do
        t  <- lookVar id
        te <- checkExp t exp
        return (SAss id te)
    
    SArrAss id e1 e2      -> do -- id[e1] = e2
        (TArr t) <- lookVar id
        te1      <- checkExp TInt e1
        te2      <- checkExp t e2 
        return $ SArrAss id te1 te2

    SNewArrAss id t exp   -> do -- id = new t[exp]
        (TArr t2) <- lookVar id
        te        <- checkExp TInt exp
        if t == t2
        then return $ SNewArrAss id t te
        else error $ "array " ++ (printTree id) ++ " of incorrect type"
    
    SIncr id              -> do
        t <- lookVar id
        case t of
            TInt -> return stm
            _    -> fail $ "Not allowed to increment variables of type " ++ printTree t
            
    SDecr id              -> do
        t <- lookVar id
        case t of
            TInt -> return stm
            _    -> fail $ "Not allowed to decrement variables of type " ++ printTree t
            
    SRet exp              -> do
        te <- checkExp rt exp
        return (SRet te)
    SVRet                 -> case rt of
        TVoid -> return stm
        _     -> fail "Not allowed to return void in non-void function."
        
    SIf exp stm1          -> do
        te    <- checkExp TBool exp
        stm1' <- checkStm rt stm1
        return (SIf te stm1')
        
    SIfElse exp stm1 stm2 -> do
        te    <- checkExp TBool exp
        stm1' <- checkStm rt stm1
        stm2' <- checkStm rt stm2
        return (SIfElse te stm1' stm2')
        
    SWhile exp stm1       -> do
        te    <- checkExp TBool exp
        stm1' <- checkStm rt stm1
        return (SWhile te stm1')
        
    SForEach t id exp stm1 -> do -- for(t id : exp) stm1
        newCxt
        extendVar id t
        te    <- checkExp (TArr t) exp
        tstm1 <- checkStm rt stm1
        rmCxt
        return $ SForEach t id te tstm1
        
    SExp exp -> do
        te <- inferExp exp
        return (SExp te)

-- checkDecl adds all declared and initialized variables to the env and checks 
-- that the expressions in the initilizations are type correct. It returns a 
-- list of the items, with the expressions of the initializations type-annotated. 
checkDecl :: Type -> [Item] -> TypeCheckM [Item]
checkDecl t []           = return []
checkDecl t (item:items) = case item of
    (IDecl id) -> do
        extendVar id t
        items' <- checkDecl t items
        return $ (IDecl id):items'
    (IInit id exp) -> do
        etype  <- checkExp t exp
        extendVar id t
        items' <- checkDecl t items
        return ((IInit id etype):items')
    (IArrInit id t2 exp) -> let (TArr t') = t in
        if t2 == t'
        then do
            etype  <- checkExp TInt exp
            extendVar id t
            items' <- checkDecl t items
            return ((IArrInit id t2 etype):items')
        else
            error $ "array " ++ (printTree id) ++ " of incorrect type"
                

----------------------- checkExp and its auxilary functions -----------------------

-- checkExp checks that the the expression has the given type. It returns the type
-- correct and type annotated expression. If it fails it returns a string with an
-- error message.
checkExp :: Type -> Exp -> TypeCheckM Exp
checkExp t exp = do
    te@(EType t2 _) <- inferExp exp
    if (t2 == t) 
        then return te
        else fail $ "Exp " ++ printTree exp ++ " has incorrect type. Expected type: " ++
            printTree t ++ ". Actual type: " ++ printTree t2

-- inferExp infers the type of the given expression and returns the type annotated
-- expression. If the expression is not type correct, it returns an error message.
inferExp :: Exp -> TypeCheckM Exp  
inferExp exp = case exp of
    EVar id        -> do
        t <- lookVar id
        return $ EType t exp
    ELit LTrue     -> return $ EType TBool exp
    ELit LFalse    -> return $ EType TBool exp
    ELit (LInt _)  -> return $ EType TInt exp
    ELit (LDoub _) -> return $ EType TDoub exp
    ELit (LStr _)  -> fail "String literals not allowed outside calls to printString"
    
    -- Check that the function call has the correct types and returns the return
    -- type of the function
    EApp id exps   -> case (\(Ident str) -> str) id of
        "printString" -> case exps of
            [ELit (LStr s)] -> return $ EType TVoid $ EApp id [(EType TStr (ELit (LStr s)))]  
            _               -> fail "Function printString called with incorrect args"
        _             -> do
            (TFun retT argTs) <- lookFuncSig id
            tes <- checkArgTypes argTs exps
            return $ EType retT (EApp id tes)
    
    EArrLen id     -> do    -- id.length
        (TArr _) <- lookVar id
        return $ EType TInt $ EArrLen id
    
    EArrInd id e   -> do    -- id[e]
        (TArr t) <- lookVar id
        te <- checkExp TInt e
        return $ EType t $ EArrInd id te
    
    ENeg e         -> do
        te@(EType t _) <- inferExp e
        case elem t [TInt, TDoub] of
            True -> return $ EType t (ENeg te)
            _    -> fail $ "Incorrect type of expression " ++ printTree e
    ENot e         -> do
        te <- checkExp TBool e
        return $ EType TBool $ ENot te
        
    EMul e1 Mod e2 -> do
        te1 <- checkExp TInt e1
        te2 <- checkExp TInt e2
        return $ EType TInt (EMul te1 Mod te2)
    
    EMul e1 op e2   -> do
        te1@(EType t _) <- inferExp e1
        case elem t [TInt, TDoub] of
            True -> do
                te2 <- checkExp t e2
                return $ EType t (EMul te1 op te2)
            _    -> fail $ "Incorrect type of expression " ++ printTree e1
    
    EAdd e1 op e2  -> do
        te1@(EType t _) <- inferExp e1
        case elem t [TInt, TDoub] of
            True -> do
                te2 <- checkExp t e2
                return $ EType t (EAdd te1 op te2)
            _    -> fail $ "Incorrect type of expression " ++ printTree e1 
    
    ERel e1 op e2  -> do
        te1@(EType t _) <- inferExp e1
        case elem t [TInt, TDoub, TBool] of
            True -> do
                te2 <- checkExp t e2
                return $ EType TBool (ERel te1 op te2)
            _    -> fail $ "Incorrect type of expression " ++ printTree e1 
            
    EAnd e1 e2     -> do
        te1 <- checkExp TBool e1
        te2 <- checkExp TBool e2
        return $ EType TBool (EAnd te1 te2)
        
    EOr e1 e2      -> do
        te1 <- checkExp TBool e1
        te2 <- checkExp TBool e2
        return $ EType TBool (EOr te1 te2)

-- checkArgTypes check that the argument types of a function match those of the
-- function signature. It either returns the type annotated, type correct argument
-- expressions or an error message.
checkArgTypes :: [Type] -> [Exp] -> TypeCheckM [Exp]
checkArgTypes []     []     = return []
checkArgTypes (t:ts) (e:es) = do
    te <- checkExp t e
    tes <- checkArgTypes ts es
    return (te:tes)
checkArgTypes _      _      = fail "Function called with incorrect nbr of args"
