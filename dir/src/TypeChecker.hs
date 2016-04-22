module TypeChecker where

import AbsJavalette
import PrintJavalette
import ErrM

import Data.Map
import qualified Data.Map as M

----------------------------------- **************  ----------------------------------
----------------------------------- NEW DATA TYPES  ----------------------------------
----------------------------------- **************  ----------------------------------

type Env = (FuncSig,[Cxt])                  -- functions and context stack
type FuncSig = Map Ident Type               -- function type signature
type Cxt = Map Ident Type                   -- variables with their types


--------------------------------- ****************** ---------------------------------
--------------------------------- AUXILIARY FUNCTIONS ---------------------------------
--------------------------------- ****************** ---------------------------------

-- lookVar returns the type of the variable in the topmost (i.e. innermost) context of 
-- the environment in which the variable exist.
lookVar :: Env -> Ident -> Err Type
lookVar (_, [])   id = Bad $ "No variable with id " ++ printTree id ++ " in the environment"
lookVar (f, c:cs) id = case M.lookup id c of
    Just t  -> Ok t
    Nothing -> lookVar (f, cs) id

-- lookFunSig returns the function type signature for the function with matching id.
lookFuncSig :: Env -> Ident -> Err Type
lookFuncSig (f, _) id = case M.lookup id f of
    Just t   -> Ok t
    Nothing  -> Bad $ "No function with id " ++ printTree id ++ " in the environment"

-- extendVar adds a new variable and its type to the topmost (i.e. innermost) context 
-- in the environment
extendVar :: Env -> Ident -> Type -> Err Env
extendVar (_, [])   id _ = Bad $ "There are no contexts without variable " ++  printTree id ++ " in the environment"
extendVar (f, c:cs) id t = case M.lookup id c of
    Just _  -> Bad $ "Variable " ++ printTree id ++ " already in context"
    Nothing -> Ok (f, (M.insert id t c):cs)

-- extendFun adds a new function and its type to the envionment
extendFuncSig :: Env -> Ident -> Type -> Err Env
extendFuncSig env@(f, cs) id t = case M.lookup id f of
    Just _  -> Bad $ "Function with id " ++ printTree id ++ " already exists"
    Nothing -> Ok ((M.insert id t f), cs)

-- newCxt adds a new, empty block/context to the environment 
newCxt  :: Env -> Env
newCxt (f, cs) = (f, (M.empty):cs)

-- emptyEnv returns an empty environment
emptyEnv  :: Env
emptyEnv = (M.empty, [])

----------------------------------- ********** -----------------------------------
----------------------------------- ADD THINGS -----------------------------------
----------------------------------- ********** -----------------------------------

-- addFtoEnv adds the function type signature of each definition to the environment 
addFtoEnv :: Env -> [Def] -> Err Env
addFtoEnv env []                       = Ok env
addFtoEnv env ((FnDef t id args _):ds) = do
    env' <- extendFuncSig env id (TFun t (Prelude.map (\(DArg t id) -> t) args))
    addFtoEnv env' ds

-- addVtoC adds all argument varibles to the topmost/innermost context in env
addVtoC :: Env -> [Arg] -> Err Env
addVtoC env []                 = Ok env 
addVtoC env ((DArg t id):args) = do
    env' <- extendVar env id t
    addVtoC env' args

---------------------------------- ************ -----------------------------------
---------------------------------- TYPECHECKING -----------------------------------
---------------------------------- ************ -----------------------------------

-- typecheck first adds all the definitions of the program along with the built in
-- functions to the environment and then makes sure all definitions are type correct.
-- It returns a type correct and type annotated program.
typecheck :: Program -> Err Program
typecheck (PProg ds) = do
    env' <- addFtoEnv emptyEnv (ds ++ builtIn)
    ds'  <- checkDefs env' ds 
    Ok $ PProg ds'

builtIn :: [Def]
builtIn = [(FnDef TVoid (Ident "printInt")    [DArg TInt (Ident "n")]  (DBlock [])), 
           (FnDef TVoid (Ident "printDouble") [DArg TDoub (Ident "x")] (DBlock [])),
           (FnDef TInt  (Ident "readInt")     []                       (DBlock [])),
           (FnDef TDoub (Ident "readDouble")  []                       (DBlock [])),
           (FnDef TVoid (Ident "printString") [DArg TStr (Ident "s")]  (DBlock []))]

-- checkDefs calls checkDef for all definitions in the program. It returns all the
-- type correct and type annotated function definitions.
checkDefs :: Env -> [Def] -> Err [Def]
checkDefs _   [] = Ok [] 
checkDefs env (d:ds) = do
    d'  <- checkDef env d
    ds' <- checkDefs env ds
    Ok $ (d':ds')

-- checkDef adds a new context to the env, adds all the func args to the context and 
-- checks that all stms in its body are type correct. It returns a type correct and
-- type annotated function definition.
checkDef :: Env -> Def -> Err Def
checkDef env (FnDef t id args (DBlock stms)) = do
    env' <- addVtoC (newCxt env) args
    stms' <- checkStms env' t stms
    Ok $ (FnDef t id args (DBlock stms'))


----------------------- checkStms and its auxilary functions ----------------------

-- checkStms calls checkStm for all statements in one definition block. It returns
-- all the type correct and type annotated statements.
checkStms :: Env -> Type -> [Stm] -> Err [Stm]
checkStms _   _ []     = Ok []
checkStms env t (s:ss) = do
    (env', s') <- checkStm env t s
    ss'        <- checkStms env' t ss
    Ok (s':ss')

-- checkStm checks that stm is type correct. It returns an updated environment and
-- the statement, with all its sub-expressions type-annotated.
checkStm :: Env -> Type -> Stm -> Err (Env, Stm)
checkStm env rt stm = case stm of
    SEmpty                -> Ok (env, stm)
    
    SBlock (DBlock stms)  -> do
        stms' <- checkStms (newCxt env) rt stms
        Ok (env, (SBlock (DBlock stms')))
        
    SDecl t items         -> do 
        (env', items') <- checkDecl env t items
        Ok (env', (SDecl t items'))
                    
    SAss id exp           -> do
        t <- lookVar env id
        te <- checkExp env t exp
        Ok (env, (SAss id te))
        
    SIncr id              -> do
        t <- lookVar env id
        case t of
            TInt -> Ok (env, stm)
            _    -> Bad $ "Not allowed to increment variables of type " ++ printTree t
            
    SDecr id              -> do
        t <- lookVar env id
        case t of
            TInt -> Ok (env, stm)
            _    -> Bad $ "Not allowed to decrement variables of type " ++ printTree t
            
    SRet exp              -> do
        te <- checkExp env rt exp
        Ok (env, (SRet te))
    SVRet                 -> case rt of
        TVoid -> Ok (env, stm)
        _     -> Bad "Not allowed to return void in non-void function."
        
    SIf exp stm1          -> do
        te         <- checkExp env TBool exp
        (_, stm1') <- checkStm env rt stm1
        Ok (env, (SIf te stm1'))
        
    SIfElse exp stm1 stm2 -> do
        te <- checkExp env TBool exp
        (_, stm1') <- checkStm env rt stm1
        (_, stm2') <- checkStm env rt stm2
        Ok (env, (SIfElse te stm1' stm2'))
        
    SWhile exp stm1 -> do
        te         <- checkExp env TBool exp
        (_, stm1') <- checkStm env rt stm1
        Ok (env, (SWhile te stm1'))
        
    SExp exp -> do
        te <- inferExp env exp
        Ok (env, (SExp te))

-- checkDecl adds all declared and initialized variables to the env and 
-- checks that the expressions in the initilizations are type correct.
-- It returns a tuple of the updated environment and a list of the items,
-- with the expressions of the initializations type-annotated. 
checkDecl :: Env -> Type -> [Item] -> Err (Env, [Item])
checkDecl env t []           = Ok (env, [])
checkDecl env t (item:items) = case item of
    (IDecl id) -> do
        env' <- extendVar env id t
        checkDecl env' t items
    (IInit id exp) -> do
        etype <- checkExp env t exp
        env' <- extendVar env id t
        (env'', items') <- checkDecl env' t items
        Ok (env'', (IInit id etype):items')

----------------------- checkExp and its auxilary functions -----------------------

-- checkExp checks that the the expression has the given type. It returns the type
-- correct and type annotated expression. If it fails it returns a string with an
-- error message.
checkExp :: Env -> Type -> Exp -> Err Exp
checkExp env t exp = do
    et@(EType t2 _) <- inferExp env exp
    if (t2 == t) 
        then Ok et
        else Bad $ "Exp " ++ printTree exp ++ " has incorrect type. Expected type: " ++
            printTree t ++ ". Actual type: " ++ printTree t2

-- inferExp either returns the type correct and type annotated expression or an
-- error message.
inferExp :: Env -> Exp -> Err Exp  
inferExp env exp = case exp of
    EVar id        -> do
        t <- lookVar env id
        Ok $ EType t exp
    ELit LTrue     -> Ok $ EType TBool exp
    ELit LFalse    -> Ok $ EType TBool exp
    ELit (LInt _)  -> Ok $ EType TInt exp
    ELit (LDoub _) -> Ok $ EType TDoub exp
    ELit (LStr _)  -> Bad "String literals not allowed outside calls to printString"
    
    -- Check that the function call has the correct types and return the return
    -- type of the function
    EApp id exps    -> case (\(Ident str) -> str) id of
        "printString" -> case exps of
            [ELit (LStr s)] -> Ok $ EType TVoid $ EApp id [(EType TStr (ELit (LStr s)))]  
            _               -> Bad "Function printString called with incorrect args"
        _             -> do
            (TFun retT argTs) <- lookFuncSig env id
            tes <- checkArgTypes env argTs exps
            Ok $ EType retT (EApp id tes)
    
    ENeg e          -> do
        (EType t _) <- inferExp env e
        case elem t [TInt, TDoub] of
            True -> Ok $ EType t (ENeg (EType t e))
            _    -> Bad $ "Incorrect type of expression " ++ printTree e
    ENot e          -> do
        te <- checkExp env TBool e
        Ok $ EType TBool te
        
    EMul e1 Mod e2 -> do
        te1 <- checkExp env TInt e1
        te2 <- checkExp env TInt e2
        Ok $ EType TInt (EMul te1 Mod te2)
    
    EMul e1 op e2   -> do
        (EType t _) <- inferExp env e1          
        case elem t [TInt, TDoub] of
            True -> do
                checkExp env t e2
                Ok $ EType t (EMul (EType t e1) op (EType t e2))
            _    -> Bad $ "Incorrect type of expression " ++ printTree e1 
    
    EAdd e1 op e2   -> do
        (EType t _) <- inferExp env e1
        case elem t [TInt, TDoub] of
            True -> do
                checkExp env t e2
                Ok $ EType t (EAdd (EType t e1) op (EType t e2))
            _    -> Bad $ "Incorrect type of expression " ++ printTree e1 
    
    ERel e1 op e2   -> do
        (EType t _) <- inferExp env e1
        case elem t [TInt, TDoub, TBool] of
            True -> do
                checkExp env t e2
                Ok $ EType TBool (ERel (EType t e1) op (EType t e2))
            _    -> Bad $ "Incorrect type of expression " ++ printTree e1 
            
    EAnd e1 e2      -> do
        te1 <- checkExp env TBool e1
        te2 <- checkExp env TBool e2
        Ok $ EType TBool (EAnd te1 te2)
        
    EOr e1 e2       -> do
        te1 <- checkExp env TBool e1
        te2 <- checkExp env TBool e2
        Ok $ EType TBool (EOr te1 te2)

-- checkArgTypes check that the argument types of a function matches those of the
-- function signature. It either returns the type annotated, type correct argument
-- expressions or an error message.
checkArgTypes :: Env -> [Type] -> [Exp] -> Err [Exp]
checkArgTypes _   []     []     = Ok []
checkArgTypes env (t:ts) (e:es) = do
    te <- checkExp env t e
    tes <- checkArgTypes env ts es
    Ok (te:tes)
checkArgTypes _   _      _      = Bad "Function called with incorrect nbr of args"
