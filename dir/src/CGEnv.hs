module CGEnv where

import Control.Monad.State
import qualified Data.Map as M

import AbsJavalette
import PrintJavalette
import LLVM


data Env = Env {
    vars         :: M.Map Ident Ident,    -- map of id1 (in Javalette) + id2 
                                          -- that points at id1
    funcs        :: M.Map Ident Type,     -- map of function id + function type
    varCounter   :: Int,                  -- the next available name for a variable
    labelCounter :: Int,                  -- the next free label number
    header       :: [HeaderInstr],        -- list of header instructions to execute
    code         :: [Instruction]         -- list of instructions to execute
}

-- newLabel returns a new label and increases labelCounter by 1
newLabel :: State Env String
newLabel = do
    env <- get
    let l = labelCounter env
    modify (\env -> env{labelCounter = l+1})
    return $ "label" ++ show l

-- newLocVar returns the next free variable name and increases varCounter by 1
newLocVar :: String -> State Env Ident
newLocVar s = do
    env <- get
    let v = varCounter env
    modify (\env -> env{varCounter = (v+1)})
    return $ Ident $ "%" ++ s ++ show v
    
newGlobVar :: State Env Ident
newGlobVar = do
    env <- get
    let v = varCounter env
    modify (\env -> env{varCounter = (v+1)})
    return $ Ident $ "@s" ++ show v

-- lookupVar returns the corresponding LLVM variable of a given Javalette variable
lookupVar :: Ident -> State Env Ident
lookupVar id = do
    env <- get
    case M.lookup id (vars env) of
        Just id2 -> return id2
        Nothing  -> error $ "no Javalette variable with id " ++ printTree id ++ " in the environment.\n"   

-- lookupFun returns the type of a function with the given id if it exists in the environment
lookupFun :: Ident -> State Env Type
lookupFun id = do
    env <- get
    case M.lookup id (funcs env) of
        Just t   -> return t
        Nothing  -> error $ "no function with id " ++ printTree id ++ " in the environment.\n"


-- extendVar maps a Javalette variable to a LLVM variable name
extendVar :: Ident -> String -> State Env Ident
extendVar id lId = do
    var <- newLocVar lId
    env <- get
    modify (\env -> env{vars = M.insert id var (vars env)})
    return var

-- extendFun adds a new function to the the environment
extendFun :: Ident -> Type -> State Env ()
extendFun id t = do
    env <- get
    modify (\env -> env{funcs = M.insert id t (funcs env)})

-- startEnv creates an empty environment with default values 
startEnv :: Env
startEnv = Env{
    vars            = M.empty,
    funcs           = 
         M.insert (Ident "printInt")    (TFun TVoid [TInt])  $
         M.insert (Ident "printDouble") (TFun TVoid [TDoub]) $
         M.insert (Ident "printString") (TFun TVoid [TStr])  $
         M.insert (Ident "readInt")     (TFun TInt  [])      $
         M.insert (Ident "readDouble")  (TFun TDoub [])      M.empty,
    varCounter      = 0,
    labelCounter    = 0,
    header          = [],
    code            = []
    }

-- addDefs takes a list of Defs and adds them to the environment
addDefs :: [Def] -> State Env ()
addDefs []                       = return ()
addDefs ((FnDef t id args _):ds) = do
    extendFun id $ TFun t $ Prelude.map (\(DArg t _) -> t) args
    addDefs ds
        
