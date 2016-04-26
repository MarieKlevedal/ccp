module CGEnv where

import Control.Monad.Reader
import Control.Monad.State

import AbsJavalette
import PrintJavalette
import ParJavalette
import LexJavalette
import ErrM
import TypeChecker hiding (Env, emptyEnv, extendVar)

import qualified Data.Map as M


------------------------------- ************** ---------------------------
------------------------------- NEW DATA TYPES ---------------------------
------------------------------- ************** ---------------------------

data Env = Env {
    filename        :: String,               -- the name of the current testfile
    vars            :: [Context],            -- a list of map's of id + address of the variable
    funcs           :: M.Map Ident Type,     -- map of function id + function type
    addrCounter     :: Int,                  -- the next available address to put a variable in
    labelCounter    :: Int,                  -- the next free label number
    code            :: [Instruction]         -- list of instructions to execute
}

type Context = M.Map Ident Int     -- the Int is the address for the variable


data Instruction =
    Comment String     |
    Text String        |
    Label String       |
    Iconst1            |
    Iconst0            |
    ILoad Int          |   --address
    IStore Int         |   --address
    Bipush Int         |
    Pop                |
    Duplicate          |
    IMul               |
    IDiv               |
    IAdd               |    
    ISub               |
    IfCmLT String      |    --label
    IfCmLTEQ String    |    --label
    IfCmEQ String      |    --label
    IfCmNEQ String     |    --label
    IfEQ String        |    --label
    Goto String        |    --label
    Return             |
    IReturn            |
    Invoke String String Type
    
-- instrToStr converts instructions to llvm command strings
instrToStr :: Instruction -> String
instrToStr (Comment str)      = "; " ++ str
instrToStr (Text str)         = str
instrToStr (Label str)        = str ++ ":"
instrToStr Iconst1            = "iconst_1"
instrToStr Iconst0            = "iconst_0"
instrToStr (Bipush x)         = "bipush " ++ (show x)
instrToStr Pop                = "pop"
instrToStr Duplicate          = "dup"
instrToStr (ILoad addr)       = "iload " ++ (show addr)
instrToStr (IStore addr)      = "istore " ++ (show addr)
instrToStr IMul               = "imul"
instrToStr IDiv               = "idiv"
instrToStr IAdd               = "iadd"
instrToStr ISub               = "isub"
instrToStr (IfCmLT label)     = "if_icmplt " ++ label
instrToStr (IfCmLTEQ label)   = "if_icmple " ++ label
instrToStr (IfCmEQ label)     = "if_icmpeq " ++ label
instrToStr (IfCmNEQ label)    = "if_icmpne " ++ label
instrToStr (IfEQ label)       = "ifeq " ++ label
instrToStr (Goto label)       = "goto " ++ label
instrToStr Return             = "return"
instrToStr IReturn            = "ireturn"
instrToStr (Invoke c f t)     = "invokestatic "


-- typeToStr converts types to llvm types
typeToStr :: Type -> String
typeToStr t = case t of
    TInt    -> "i32"
    TDoub   -> "double"
    TBool   -> "i1"
    TStr -> "i8*"
    TVoid   -> "void"
    
-------------------------- ********************* ------------------------
-------------------------- ENV RELATED FUNCTIONS ------------------------
-------------------------- ********************* ------------------------

-- newLabel returns a new label and increases labelCounter with 1
newLabel :: State Env String
newLabel = do
    env <- get
    let l = labelCounter env
    modify (\env -> env{labelCounter = l+1})
    return $ "L" ++ show l

-- newAddr returns the next free address and then increases addrCounter with 1
newAddr :: State Env Int
newAddr = do
    env <- get
    let v = addrCounter env
    modify (\env -> env{addrCounter = (v+1)})
    return v

-- lookupVar returns the address of a variable in the environment, in the
-- topmost/innermost context in which it exists
lookupVar :: Ident -> State Env Int
lookupVar id = do
    env <- get    
    return (lookupVar' id (vars env))

-- lookupVar' is a help function to lookupVar, that enables the recursion
lookupVar' :: Ident -> [Context] -> Int
lookupVar' id []     = error $ "no variable with id " ++ printTree id ++ " in the environment.\n"
lookupVar' id (c:cs) = case M.lookup id c of
    Just i  -> i
    Nothing -> lookupVar' id cs

-- lookupFun returns the type of a function with the given id if it exists in the environment
lookupFun :: Ident -> State Env Type
lookupFun id = do
    env <- get
    case M.lookup id (funcs env) of
        Just t   -> return t
        Nothing  -> error $ "no function with id " ++ printTree id ++ " in the environment.\n"

-- extendVar adds a new variable id to the current context
extendVar :: Ident -> State Env ()
extendVar id = do
    addr <- newAddr
    env <- get
    let c:cs = vars env
    modify (\env -> env{vars = (M.insert id addr c):cs})

-- extendFun adds a new function to the the environment
extendFun :: Ident -> Type -> State Env ()
extendFun id t = do
    env <- get
    modify (\env -> env{funcs = (M.insert id t (funcs env))})

-- newBlock adds a new block/context to the environment
newBlock :: State Env ()
newBlock = do
    env <- get
    modify (\env -> env{vars = (M.empty):(vars env)})

-- exitBlock removes the topmost/innermost block/context from the environment
exitBlock :: State Env ()
exitBlock = do
    env <- get
    modify (\env -> env{vars = tail(vars env)})

-- emptyEnv creates an empty environment with default values 
emptyEnv :: String -> Env
emptyEnv fname = Env{
        filename        = fname,
        vars            = [],
        funcs           = M.empty,
        addrCounter     = 0,
        labelCounter    = 0,
        code            = []
        }

-- addDefs takes a list of Defs and adds them to the environment
addDefs :: [Def] -> State Env ()
addDefs []                         = return ()
addDefs ((FnDef t id args _):defs) = do
    extendFun id $ TFun t $ Prelude.map (\(DArg t _) -> t) args
    addDefs defs
        
