module TypeChecker where

import AbsJavalette
import PrintJavalette
import ErrM

import Data.Map
import qualified Data.Map as M

----------------------------------- **************  ----------------------------------
----------------------------------- NEW DATA TYPES  ----------------------------------
----------------------------------- **************  ----------------------------------

type Env = (Sig,[Cxt])                  -- functions and context stack
type Sig = Map Ident ([Type],Type)      -- function type signature
type Cxt = Map Ident Type               -- variables with their types


--------------------------------- ****************** ---------------------------------
--------------------------------- AUXILARY FUNCTIONS ---------------------------------
--------------------------------- ****************** ---------------------------------

-- lookVar returns the type of the variable in the topmost (i.e. innermost) context of 
-- the environment in which the variable exists
lookVar :: Env -> Ident -> Err Type
lookVar (_, [])   id = Bad $ "no variable with id " ++ printTree id ++ " in the environment"
lookVar (s, c:cs) id = case M.lookup id c of
    Just t  -> Ok t
    Nothing -> lookVar (s, cs) id

-- lookFun returns a tuple with a list of the types of the argument list and the
-- return type for the function with that id 
lookFun :: Env -> Ident -> Err ([Type],Type)
lookFun (s, _) id = case M.lookup id s of
    Just tup -> Ok tup
    Nothing  -> Bad $ "no function with id " ++ printTree id ++ " in the environment"

-- extendVar adds a new variable and its type to the topmost (i.e. innermost) context 
-- in the environment
extendVar :: Env -> Ident -> Type -> Err Env
extendVar (_, [])   id _ = Bad $ "there are no contexts without variable " ++  printTree id ++ " in the environment"
extendVar (s, c:cs) id t = case M.lookup id c of
    Just _  -> Bad $ "variable " ++ printTree id ++ " already in context"
    Nothing -> Ok (s, (M.insert id t c):cs)

-- extendFun adds a new funcion and its types to the envionment
extendFun :: Env -> Ident -> ([Type], Type) -> Err Env
extendFun env@(s, cs) id tup = case M.lookup id s of
    Just _  -> Bad $ "function with id " ++ printTree id ++ " already exists"
    Nothing -> Ok ((M.insert id tup s), cs)

-- newBlock adds a new, empty block/context to the environment 
newBlock  :: Env -> Env
newBlock (s, cs) = (s, (M.empty):cs)

-- emptyEnv returns an empty environment
emptyEnv  :: Env
emptyEnv = (M.empty, [])

----------------------------------- ********** -----------------------------------
----------------------------------- ADD THINGS -----------------------------------
----------------------------------- ********** -----------------------------------

-- addFtoC adds the function type signature of each definition to the environment 
addFtoC :: Env -> [Def] -> Err Env
addFtoC env []                      = Ok env
addFtoC env ((FnDef t id args _):ds) = 
    case extendFun env id ((Prelude.map argtoT args), t) of
        Ok env' -> addFtoC env' ds
        Bad str -> Bad str

-- argtoT takes an argument and returns its type
argtoT :: Arg -> Type
argtoT (DArg t id) = t

-- addVtoC adds all argument varibles to the topmost/innermost context in env
addVtoC :: Env -> [Arg] -> Err Env
addVtoC env []                  = Ok env 
addVtoC env ((DArg t id):args) = case extendVar env id t of
    Ok env' -> addVtoC env' args
    Bad str -> Bad str

---------------------------------- ************ -----------------------------------
---------------------------------- TYPECHECKING -----------------------------------
---------------------------------- ************ -----------------------------------

-- typecheck first adds all the definitions of the program to the environment and then
-- makes sure all definitions are type correct
typecheck :: Program -> Err ()
typecheck (PProg ds) = do 
    let builtIn = [(FnDef TVoid   (Ident "printInt")    [DArg TInt (Ident "x")]    (DBlock [])), 
                   (FnDef TVoid   (Ident "printDouble") [DArg TDoub (Ident "x")] (DBlock [])),
                   (FnDef TInt    (Ident "readInt")     []                           (DBlock [])),
                   (FnDef TDoub (Ident "readDouble")  []                           (DBlock []))]
    case addFtoC emptyEnv (ds ++ builtIn) of
        Ok env' -> callCheckDef env' ds 
        Bad str -> Bad str

-- callCheckDef calls checkDef for all defs in the program
callCheckDef :: Env -> [Def] -> Err ()
callCheckDef _   [] = Ok () 
callCheckDef env (d:ds) = do
    checkDef env d
    callCheckDef env ds

-- checkDef adds a new context to the env, adds all the func args to the context and 
-- checks that all stms in its body are type correct
checkDef :: Env -> Def -> Err ()
checkDef env (FnDef t id args (DBlock stms)) = do
    case addVtoC (newBlock env) args of
        Ok env' -> checkStms env' t stms
        Bad str -> Bad str

----------------------- checkStms and its auxilary functions ----------------------

-- checkStms calls checkStm for all stms in one def
checkStms :: Env -> Type -> [Stm] -> Err ()
checkStms _   _ []     = Ok ()
checkStms env t (s:ss) = case checkStm env t s of
    Ok env' -> checkStms env' t ss
    Bad str -> Bad str

-- checkStm checks that stm is type correct
checkStm :: Env -> Type -> Stm -> Err Env
checkStm env rt stm = case stm of
    SEmpty                -> Ok env
    SBlock (DBlock stms)  -> do
        checkStms (newBlock env) rt stms
        Ok env
    SDecl t items         -> checkDecl env t items
    SAss id exp           -> do
        t <- lookVar env id
        checkExp env t exp
        Ok env
    SIncr id              -> do
        t <- lookVar env id
        case t of
            TInt -> Ok env
            _    -> Bad "can only increment Int"
    SDecr id              -> do
        t <- lookVar env id
        case t of
            TInt -> Ok env
            _    -> Bad "can only decrement Int"
    SRet exp              -> do
        checkExp env rt exp
        Ok env
    SVRet                 -> case rt of
        TVoid -> Ok env
        _     -> Bad "return type is not void"
    SIf exp stm           -> do
        checkExp env TBool exp
        checkStm env rt stm
    SIfElse exp stm1 stm2 -> do
        checkExp env TBool exp
        checkStm env rt stm1
        checkStm env rt stm2
    SWhile exp stm -> do
        checkExp env TBool exp
        checkStm env rt stm
    SExp exp -> do
        inferExp env exp
        Ok env

checkDecl :: Env -> Type -> [Item] -> Err Env
checkDecl env t []           = Ok env
checkDecl env t (item:items) = case item of
    (IDecl id) -> case extendVar env id t of
        Ok env' -> checkDecl env' t items
        Bad str -> Bad str
    (IInit id exp) -> do
        checkExp env t exp
        case extendVar env id t of
            Ok env' -> checkDecl env' t items
            Bad str -> Bad str

----------------------- checkExp and its auxilary functions -----------------------

-- checkExp checks that the the expression has the given type
checkExp :: Env -> Type -> Exp -> Err ()
checkExp env t exp = do
    t2 <- inferExp env exp
    if (t2 == t) 
        then Ok ()
        else Bad $ "type of " ++ printTree exp

-- inferExp returns the type of exp
inferExp :: Env -> Exp -> Err Type  
inferExp env exp = case exp of
    EVar id        -> lookVar env id
    ELit LTrue     -> Ok TBool
    ELit LFalse    -> Ok TBool
    ELit (LInt n)  -> Ok TInt
    ELit (LDoub n) -> Ok TDoub

    
    -- Check that the function call has the correct types and return the return
    -- type of the function
    EApp id exps    -> case lookFun env id of
        Ok (argTs, retT) -> 
            if length exps == length argTs && (checkArgTypes env exps []) == argTs 
                then Ok retT
                else Bad $ "non-matching argument lists for function " ++ printTree id
        Bad _            -> Bad $ "function " ++ printTree id ++ " not defined"
    Neg e          -> do
        t <- inferExp env e
        case elem t [TInt, TDoub] of
            True -> Ok t
            _    -> Bad $ "incorrect type of expression " ++ printTree e
    Not e          -> do
        checkExp env TBool e
        Ok TBool
    EMul e1 Mod e2 -> checkMatchingType env [TInt] e1 e2
    EMul e1 _ e2   -> checkMatchingType env [TInt, TDoub] e1 e2
    EAdd e1 _ e2   -> checkMatchingType env [TInt, TDoub] e1 e2
    ERel e1 _ e2   -> do
        t <- inferExp env e1
        case elem t [TInt, TDoub, TBool] of
            True -> do
                checkExp env t e2
                Ok TBool
            _    -> Bad $ "incorrect type of expression " ++ printTree e1 
    EAnd e1 e2      -> do
        checkExp env TBool e1
        checkExp env TBool e2
        Ok TBool 
    EOr e1 e2       -> do
        checkExp env TBool e1
        checkExp env TBool e2
        Ok TBool

-- checkArgTypes calls inferExp on all the expressions and returns a list of the 
-- types to the expressions for which it succeded
checkArgTypes :: Env -> [Exp] -> [Type] -> [Type]
checkArgTypes _   []     _  = []
checkArgTypes env (e:es) ts = case inferExp env e of
    Ok t  -> (checkArgTypes env es ts) ++ [t]
    Bad _ -> checkArgTypes env es ts
    
-- checkArithmType checks that the two expressions have the same type and that the
-- type is one of int and double
checkMatchingType :: Env -> [Type] -> Exp -> Exp -> Err Type
checkMatchingType env ts e1 e2 = do
    t <- inferExp env e1
    case elem t ts of
        True -> do
            checkExp env t e2
            Ok t
        _    -> Bad $ "incorrect type of expression " ++ printTree e1 

-- checkCompType checks that the two expressions have the same type (one of int, 
-- double and bool) and returns TBool at success
checkCompType :: Env -> Exp -> Exp -> Err Type
checkCompType env e1 e2 = do
    t <- inferExp env e1
    if elem t [TInt, TDoub, TBool]
        then do
            checkExp env t e2
            Ok TBool
        else
            Bad $ "type of expression " ++ printTree e1
