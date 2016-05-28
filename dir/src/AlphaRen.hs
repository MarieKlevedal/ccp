module AlphaRen where

import AbsJavalette
import PrintJavalette
import ErrM

import Data.Map
import qualified Data.Map as M
import Data.Maybe
import Control.Monad.State

data Env = Env {
    funcs  :: FuncSig,
    cxts   :: [Cxt],
    fCount :: Int,
    vCount :: Int
}                  
type FuncSig = Map Ident Ident
type Cxt = Map Ident Ident

type AlphaRenM a = StateT Env Err a

----------------------------------------------------------------------------
startEnv :: Env
startEnv = Env {
    funcs  = M.insert (Ident "printInt")    (Ident "printInt") $
             M.insert (Ident "printDouble") (Ident "printDouble") $
             M.insert (Ident "printString") (Ident "printString") $
             M.insert (Ident "readInt")     (Ident "readInt") $
             M.insert (Ident "readDouble")  (Ident "readDouble") M.empty,
    cxts   = [],
    fCount = 0,
    vCount = 0
} 

newCxt :: AlphaRenM ()
newCxt = modify (\env -> env{cxts = (M.empty):(cxts env)})

rmCxt :: AlphaRenM ()
rmCxt = modify (\env -> env{cxts = tail (cxts env)})

newVar :: AlphaRenM Ident
newVar = do
    env <- get
    let vc = vCount env
    let newId  = Ident $ "%v" ++ show vc
    modify (\env -> env{vCount = (vc+1)})
    return newId

-- addVToEnv gives the given JavaLette variable a correspoding LLVM variable and 
-- adds the pair to the env.
addVToEnv :: Ident -> AlphaRenM Ident
addVToEnv id = do
    newId <- newVar
    env <- get
    let (c:cs) = cxts env
    modify (\env -> env{cxts = (M.insert id newId c):cs})
    return newId

-- addFToEnv maps the given (old) id to the next free function id. The "main"
-- function keeps its old name.
addFToEnv :: Ident -> AlphaRenM Ident
addFToEnv id@(Ident "main") = do
    env <- get
    modify (\env -> env{funcs = M.insert id id (funcs env)})
    return id
addFToEnv id                = do
    env <- get
    let fc = fCount env
    let newId  = Ident $ "f" ++ show fc
    modify (\env -> env{fCount = (fc+1)})
    modify (\env -> env{funcs = M.insert id newId (funcs env)})
    return newId

-- lookupVar returns the correspondig LLVM varible for a given JavaLette variable
lookupVar :: Ident -> AlphaRenM Ident
lookupVar id = do
    env <- get
    let newId = searchCxts (cxts env) id
    return newId

-- searchCxts is an auxiliary function of lookupVar    
searchCxts :: [Cxt] -> Ident -> Ident
searchCxts []     id = error $ "variable " ++ printTree id ++ " not in scope"
searchCxts (c:cs) id = case M.lookup id c of
    Just newId  -> newId
    Nothing     -> searchCxts cs id

-- lookupFun returns the corresponding LLVM function name to a given JavaLette
-- function name
lookupFun :: Ident -> AlphaRenM Ident
lookupFun id = do
    env <- get
    let newId = fromJust $ M.lookup id $ funcs env
    return newId
    
----------------------------------------------------------------------------

-- renameFuncNames renames the function names
renameFuncNames :: [Def] -> [Def] -> AlphaRenM [Def]
renameFuncNames []                          newDs = return newDs
renameFuncNames (d@(FnDef t id as b):oldDs) newDs = do
    newId <- addFToEnv id
    let d' = (FnDef t newId as b)
    renameFuncNames oldDs (d':newDs)

----------------------------------------------------------------------------

-- alphaRen takes a JavaLette program and returns the program with all variables
-- and functions renamed
alphaRen :: Program -> Err Program
alphaRen (PProg ds) = do
    (ds', _) <- runStateT ((renameFuncNames ds []) >>= (renameDefs . reverse)) startEnv
    return $ PProg ds'

-- renameDefs renames all the variables in every def
renameDefs :: [Def] -> AlphaRenM [Def]
renameDefs []     = return []
renameDefs (d:ds) = do
    d'  <- renameDef d
    ds' <- renameDefs ds
    return (d':ds')

-- renameDef renames all the variables in the def
renameDef :: Def -> AlphaRenM Def
renameDef d@(FnDef t id as b@(DBlock ss)) = do
    newCxt
    as' <- renameArgs as []
    ss' <- renameStms ss
    rmCxt
    return $ FnDef t id (reverse as') (DBlock ss')


-- renameArgs renames the arguments of a function
renameArgs :: [Arg] -> [Arg] -> AlphaRenM [Arg]
renameArgs []                     newAVs = return newAVs
renameArgs (a@(DArg t id):oldAVs) newAVs = do
    newId <- addVToEnv id
    let av' = DArg t newId
    renameArgs oldAVs (av':newAVs)
          
-- renameStms renames all the variables in all the statements of a list          
renameStms :: [Stm] -> AlphaRenM [Stm]
renameStms []     = return []
renameStms (s:ss) = do
    s'  <- renameStm s
    ss' <- renameStms ss
    return (s':ss')

-- renameStm renames all variables of a statement
renameStm :: Stm -> AlphaRenM Stm

renameStm SEmpty               = return SEmpty

renameStm (SBlock (DBlock ss)) = do
    newCxt
    ss' <- renameStms ss
    rmCxt
    return $ SBlock $ DBlock ss'   
     
renameStm (SDecl t items)      = do 
    items' <- renameDecl items
    return $ SDecl t items'
    
renameStm (SAss id exp)        = do 
    id'  <- lookupVar id
    exp' <- renameExp exp
    return $ SAss id' exp'      
          
renameStm (SArrAss id e1 e2)   = do  -- id[e1] = e2
    id' <- lookupVar id
    e1' <- renameExp e1
    e2' <- renameExp e2
    return $ SArrAss id' e1' e2'
          
renameStm (SNewArrAss id t e)  = do  -- id = new t[e]
    id' <- lookupVar id
    e'  <- renameExp e
    return $ SNewArrAss id' t e'
    
renameStm (SIncr id)           = do
    id' <- lookupVar id
    return $ SIncr id'

renameStm (SDecr id)           = do
    id' <- lookupVar id
    return $ SDecr id'

renameStm (SRet exp)           = do
    exp' <- renameExp exp
    return $ SRet exp'

renameStm SVRet                = return SVRet

renameStm (SIf exp s)          = do 
    exp' <- renameExp  exp
    s'   <- renameStm  s
    return $ SIf exp' s'
    
renameStm (SIfElse exp s1 s2)  = do
    exp' <- renameExp exp
    s1'  <- renameStm s1
    s2'  <- renameStm s2
    return $ SIfElse exp' s1' s2'
    
renameStm (SWhile exp s)       = do 
    exp' <- renameExp  exp
    s'   <- renameStm  s
    return $ SWhile exp' s'
          
renameStm (SForEach t id e s)  = do -- for(t id : e) s
    newCxt
    id' <- addVToEnv id
    e'  <- renameExp e
    s'  <- renameStm s
    rmCxt
    return $ SForEach t id' e' s'
          
renameStm (SExp exp)           = do
    exp' <- renameExp exp
    return  $ SExp exp'

 
renameDecl :: [Item] -> AlphaRenM [Item]
renameDecl []           = return []
renameDecl (item:items) = case item of
    IDecl id          -> do
        id'    <- addVToEnv id
        items' <- renameDecl items
        return $ (IDecl id'):items'
    IInit id exp      -> do
        exp'   <- renameExp exp
        id'    <- addVToEnv id
        items' <- renameDecl items
        return $ (IInit id' exp'):items'
    IArrInit id t exp -> do  -- id = new t [exp]
        exp'   <- renameExp exp
        id'    <- addVToEnv id
        items' <- renameDecl items
        return $ (IArrInit id' t exp'):items'
 
 
-- renameExp renames all the variables of an expression 
renameExp :: Exp -> AlphaRenM Exp
renameExp (EVar id)       = do
    id' <- lookupVar id
    return $ EVar $ id'
renameExp (ELit l)        = return $ ELit l
renameExp (EApp id es)    = do 
    id' <- lookupFun id
    es' <- mapM (\e -> renameExp e) es
    return $ EApp id' es'
renameExp (EArrLen id)    = do -- id.length
    id' <- lookupVar id
    return $ EArrLen id'    
renameExp (EArrInd id e)  = do -- id[e]
    id' <- lookupVar id
    e'  <- renameExp e
    return $ EArrInd id' e' 
renameExp (ENeg e)        = do
    e' <- renameExp e
    return $ ENeg e'
renameExp (ENot e)        = do
    e' <- renameExp e
    return $ ENot e'
renameExp (EMul e1 op e2) = do
    e1' <- renameExp e1
    e2' <- renameExp e2
    return $ EMul e1' op e2'
renameExp (EAdd e1 op e2) = do
    e1' <- renameExp e1
    e2' <- renameExp e2
    return $ EAdd e1' op e2'
renameExp (ERel e1 op e2) = do
    e1' <- renameExp e1
    e2' <- renameExp e2
    return $ ERel e1' op e2'
renameExp (EAnd e1 e2)    = do
    e1' <- renameExp e1
    e2' <- renameExp e2
    return $ EAnd e1' e2'
renameExp (EOr e1 e2)     = do
    e1' <- renameExp e1
    e2' <- renameExp e2
    return $ EOr e1' e2'
renameExp (EType t e)     = do
    e' <- renameExp e
    return $ EType t e'


