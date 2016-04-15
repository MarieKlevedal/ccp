module TypeChecker where

import AbsJavalette
import PrintJavalette
import ErrM

import Data.Map
import qualified Data.Map as M

----------------------------------- **************  ----------------------------------
----------------------------------- NEW DATA TYPES  ----------------------------------
----------------------------------- **************  ----------------------------------

type Env = (Sig,[Context])              -- functions and context stack
type Sig = Map Id ([Type],Type)         -- function type signature
type Context = Map Id Type              -- variables with their types


--------------------------------- ****************** ---------------------------------
--------------------------------- AUXILARY FUNCTIONS ---------------------------------
--------------------------------- ****************** ---------------------------------

-- lookVar returns the type of the variable in the topmost (i.e. innermost) context of 
-- the environment in which the variable exists
lookVar :: Env -> Id -> Err Type
lookVar (_, [])   id = Bad $ "no variable with id " ++ printTree id ++ " in the environment"
lookVar (s, c:cs) id = case M.lookup id c of
    Just t  -> Ok t
    Nothing -> lookVar (s, cs) id

-- lookFun returns a tuple with a list of the types of the argument list and the
-- return type for the function with that id 
lookFun :: Env -> Id -> Err ([Type],Type)
lookFun (s, _) id = case M.lookup id s of
    Just tup -> Ok tup
    Nothing  -> Bad $ "no function with id " ++ printTree id ++ " in the environment"

-- extendVar adds a new variable and its type to the topmost (i.e. innermost) context 
-- in the environment
extendVar :: Env -> Id -> Type -> Err Env
extendVar (_, [])   id _ = Bad $ "there are no contexts without variable " ++  printTree id ++ " in the environment"
extendVar (s, c:cs) id t = case M.lookup id c of
    Just _  -> Bad $ "variable " ++ printTree id ++ " already in context"
    Nothing -> Ok (s, (M.insert id t c):cs)

-- extendFun adds a new funcion and its types to the envionment
extendFun :: Env -> Id -> ([Type], Type) -> Err Env
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
addFtoC env ((DFun t id args _):ds) = 
    case extendFun env id ((Prelude.map argtoT args), t) of
        Ok env' -> addFtoC env' ds
        Bad str -> Bad str

-- argtoT takes an argument and returns its type
argtoT :: Arg -> Type
argtoT (ADecl t id) = t

-- addVtoC adds all argument varibles to the topmost/innermost context in env
addVtoC :: Env -> [Arg] -> Err Env
addVtoC env []                  = Ok env 
addVtoC env ((ADecl t id):args) = case extendVar env id t of
    Ok env' -> addVtoC env' args
    Bad str -> Bad str

---------------------------------- ************ -----------------------------------
---------------------------------- TYPECHECKING -----------------------------------
---------------------------------- ************ -----------------------------------

-- typecheck first adds all the definitions of the program to the environment and then
-- makes sure all definitions are type correct
typecheck :: Program -> Err ()
typecheck (PDefs ds) = do 
    let builtIn = [(DFun Type_void   (Id "printInt")    [ADecl Type_int (Id "x")]    []), 
                   (DFun Type_void   (Id "printDouble") [ADecl Type_double (Id "x")] []),
                   (DFun Type_int    (Id "readInt")     []                           []),
                   (DFun Type_double (Id "readDouble")  []                           [])]
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
checkDef env (DFun t id args stms) = do
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
    SExp exp -> do
        inferExp env exp
        Ok env
    SDecls t ids -> 
        giveIdType env t ids
    SInit t id exp -> do
        checkExp env t exp
        giveIdType env t [id]
    SReturn exp -> do
        checkExp env rt exp
        Ok env
    SWhile exp stm2 -> do
        checkExp env Type_bool exp
        checkStm env rt stm2
    SBlock stms -> do
        checkStms (newBlock env) rt stms
        Ok env
    SIfElse exp stm1 stm2 -> do
        checkExp env Type_bool exp
        checkStm env rt stm1
        checkStm env rt stm2

-- giveIdType gives all the ids in the list the given type
giveIdType :: Env -> Type -> [Id] -> Err Env
giveIdType env _ []       = Ok env
giveIdType env t (id:ids) = case extendVar env id t of
    Ok env' -> giveIdType env' t ids
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
    ETrue           -> Ok Type_bool
    EFalse          -> Ok Type_bool
    EInt n          -> Ok Type_int
    EDouble n       -> Ok Type_double
    EId id          -> lookVar env id
    
    -- Check that the function call has the correct types and return the return
    -- type of the function
    EApp id exps    -> case lookFun env id of
        Ok (argTs, retT) -> 
            if length exps == length argTs && (checkArgTypes env exps []) == argTs 
                then Ok retT
                else Bad $ "non-matching argument lists for function " ++ printTree id
        Bad _            -> Bad $ "function " ++ printTree id ++ " not defined"

    EPostIncr e     -> checkIntDouble env e
    EPostDecr e     -> checkIntDouble env e
    EPreIncr e      -> checkIntDouble env e
    EPreDecr e      -> checkIntDouble env e

    ETimes e1 e2    -> checkArithmType env e1 e2 
    EDiv e1 e2      -> checkArithmType env e1 e2 
    EPlus e1 e2     -> checkArithmType env e1 e2 
    EMinus e1 e2    -> checkArithmType env e1 e2 

    ELt e1 e2       -> checkCompType env e1 e2 
    EGt e1 e2       -> checkCompType env e1 e2 
    ELtEq e1 e2     -> checkCompType env e1 e2 
    EGtEq e1 e2     -> checkCompType env e1 e2 
    EEq e1 e2       -> checkCompType env e1 e2 
    ENEq e1 e2      -> checkCompType env e1 e2 

    EAnd e1 e2      -> do
        checkExp env Type_bool e1
        checkExp env Type_bool e2
        Ok Type_bool 
    EOr e1 e2       -> do
        checkExp env Type_bool e1
        checkExp env Type_bool e2
        Ok Type_bool 

    EAss (EId id) e2 -> do
        t <- lookVar env id
        checkExp env t e2
        Ok t

-- checkArgTypes calls inferExp on all the expressions and returns a list of the 
-- types to the expressions for which it succeded
checkArgTypes :: Env -> [Exp] -> [Type] -> [Type]
checkArgTypes _   []     _  = []
checkArgTypes env (e:es) ts = case inferExp env e of
    Ok t  -> (checkArgTypes env es ts) ++ [t]
    Bad _ -> checkArgTypes env es ts

-- checkIntDouble checks that an exp is of type int or double
checkIntDouble :: Env -> Exp -> Err Type
checkIntDouble env e = do
    t <- inferExp env e
    if elem t [Type_int, Type_double] 
        then Ok t
        else Bad $ "type of expression " ++ printTree e 
    
-- checkArithmType checks that the two expressions have the same type and that the
-- type is one of int and double
checkArithmType :: Env -> Exp -> Exp -> Err Type
checkArithmType env e1 e2 = do
    t <- inferExp env e1
    if elem t [Type_int, Type_double]
        then do
            checkExp env t e2
            Ok t
        else
            Bad $ "type of expression " ++ printTree e1 

-- checkCompType checks that the two expressions have the same type (one of int, 
-- double and bool) and returns Type_bool at success
checkCompType :: Env -> Exp -> Exp -> Err Type
checkCompType env e1 e2 = do
    t <- inferExp env e1
    if elem t [Type_int, Type_double, Type_bool]
        then do
            checkExp env t e2
            Ok Type_bool
        else
            Bad $ "type of expression " ++ printTree e1
