module AlphaRen where

import AbsJavalette
import PrintJavalette

import Data.Map
import qualified Data.Map as M
import Data.Maybe

data Env = Env {
    funcs  :: FuncSig,
    cxts   :: [Cxt],
    fCount :: Int,
    vCount :: Int
}                  
type FuncSig = Map Ident Ident
type Cxt = Map Ident Ident

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

newCxt  :: Env -> Env
newCxt env = env{cxts = (M.empty):(cxts env)}

rmCxt :: Env -> Env
rmCxt env = env{cxts = tail (cxts env)}

addVToEnv :: Env -> Ident -> (Env, Ident)
addVToEnv env id = (env'', newId) where
    env'   = env{vCount = (vCount env)+1}
    newId  = Ident $ "%v" ++ show (vCount env')
    (c:cs) = cxts env'
    c'     = M.insert id newId c
    env''  = env'{cxts = (c':cs)}

-- addFToEnv maps the given (old) id to the next free function id. The "main"
-- function keeps its old name.
addFToEnv :: Env -> Ident -> (Env, Ident)
addFToEnv env id@(Ident "main") = (env', id) where
    f      = funcs env
    f'     = M.insert id id f
    env'  = env{funcs = f'}
addFToEnv env id                = (env'', newId) where
    env'   = env{fCount = (fCount env)+1}
    newId  = Ident $ "f" ++ show (fCount env')
    f      = funcs env'
    f'     = M.insert id newId f
    env''  = env'{funcs = f'}

lookupVar :: Env -> Ident -> Ident
lookupVar env id = searchCxts (cxts env) id
    
searchCxts :: [Cxt] -> Ident -> Ident
searchCxts []     id = error $ "variable " ++ printTree id ++ " not in scope"
searchCxts (c:cs) id = case M.lookup id c of
    Just newId  -> newId
    Nothing     -> searchCxts cs id

lookupFun :: Env -> Ident -> Ident
lookupFun env id = fromJust $ M.lookup id $ funcs env

----------------------------------------------------------------------------

-- renameFuncNames renames the function names
renameFuncNames :: Env -> [Def] -> [Def] -> (Env, [Def])
renameFuncNames env []                          newDs = (env, newDs)
renameFuncNames env (d@(FnDef t id as b):oldDs) newDs = 
    renameFuncNames env' oldDs (d':newDs)
        where (env', newId) = addFToEnv env id 
              d'            = (FnDef t newId as b)

----------------------------------------------------------------------------

alphaRen :: Program -> Program
alphaRen (PProg ds) = PProg newDs'
    where (env, newDs)  = renameFuncNames startEnv ds [] 
          (_  , newDs') = renameDefs env (reverse newDs)


-- renameDefs renames all the variables in every def
renameDefs :: Env -> [Def] -> (Env, [Def])
renameDefs env []     = (env, [])
renameDefs env (d:ds) = (env'', (d':ds'))
    where (env' , d')  = renameDef env d
          (env'', ds') = renameDefs env' ds

-- renameDef renames all the variables in the def
renameDef :: Env -> Def -> (Env, Def)
renameDef env d@(FnDef t id as b@(DBlock ss)) = 
    ((rmCxt env''), (FnDef t id (reverse as') (DBlock ss')))
        where (env' , as') = renameArgs (newCxt env) as []
              (env'', ss') = renameStms env' ss

-- renameArgs renames the arguments of a function
renameArgs :: Env -> [Arg] -> [Arg] -> (Env, [Arg])
renameArgs env []                     newAVs = (env, newAVs)
renameArgs env (a@(DArg t id):oldAVs) newAVs = renameArgs env' oldAVs (av':newAVs)
    where (env', newId) = addVToEnv env id
          av'           = (DArg t newId)
          
          
renameStms :: Env -> [Stm] -> (Env, [Stm])
renameStms env []     = (env, [])
renameStms env (s:ss) = (env'', (s':ss'))
    where (env' , s')  = renameStm env s
          (env'', ss') = renameStms env' ss

renameStm :: Env -> Stm -> (Env, Stm)
renameStm env SEmpty               = (env, SEmpty)
renameStm env (SBlock (DBlock ss)) = ((rmCxt env'), (SBlock (DBlock ss'))) 
    where (env', ss') = renameStms (newCxt env) ss
renameStm env (SDecl t items)      = (env', (SDecl t items'))
    where (env', items') = renameDecl env items
renameStm env (SAss id exp)        = (env, (SAss id' exp'))
    where id'  = lookupVar env id
          exp' = renameExp env exp
renameStm env (SArrAss id e1 e2)   = (env, (SArrAss id' e1' e2')) -- id[e1] = e2
    where id' = lookupVar env id
          e1' = renameExp env e1
          e2' = renameExp env e2
renameStm env (SNewArrAss id t e)  = (env, (SNewArrAss id' t e')) -- id = new t[e]
    where id' = lookupVar env id
          e'  = renameExp env e
renameStm env (SIncr id)           = (env, (SIncr (lookupVar env id))) 
renameStm env (SDecr id)           = (env, (SDecr (lookupVar env id))) 
renameStm env (SRet exp)           = (env, (SRet (renameExp env exp)))
renameStm env SVRet                = (env, SVRet)
renameStm env (SIf exp s)          = (env', (SIf exp' s'))
    where exp'       = renameExp env exp
          (env', s') = renameStm env s
renameStm env (SIfElse exp s1 s2)  = (env'', (SIfElse exp' s1' s2'))
    where exp'         = renameExp env exp
          (env' , s1') = renameStm env s1
          (env'', s2') = renameStm env' s2
renameStm env (SWhile exp s)       = (env', (SWhile exp' s'))
    where exp'       = renameExp env exp
          (env', s') = renameStm env s
renameStm env (SForEach t id e s)  = ((rmCxt env''), (SForEach t id' e' s')) 
    -- for(t id : e) s
    where (env', id') = addVToEnv (newCxt env) id
          e'          = renameExp env' e
          (env'', s') = renameStm env' s
renameStm env (SExp exp)           = (env, (SExp (renameExp env exp)))
 
renameDecl :: Env -> [Item] -> (Env, [Item])
renameDecl env []           = (env, [])
renameDecl env (item:items) = case item of
    (IDecl id)          -> (env'', (IDecl id'):items')
        where (env' , id')    = addVToEnv env id
              (env'', items') = renameDecl env' items
    (IInit id exp)      -> (env'', (IInit id' exp'):items')
        where exp'            = renameExp env exp
              (env' , id')    = addVToEnv env id
              (env'', items') = renameDecl env' items
    (IArrInit id t exp) -> (env'', (IArrInit id' t exp'):items') -- id = new t [exp]
        where exp'            = renameExp env exp
              (env' , id')    = addVToEnv env id
              (env'', items') = renameDecl env' items
 
 
renameExp :: Env -> Exp -> Exp
renameExp env (EVar id)       = EVar $ lookupVar env id
renameExp env (ELit l)        = ELit l
renameExp env (EApp id es)    = EApp id' es'
    where id' = lookupFun env id
          es' = Prelude.map (\e -> renameExp env e) es
renameExp env (EArrLen id)    = EArrLen $ lookupVar env id    -- id.length
renameExp env (EArrInd id e)  = EArrInd (lookupVar env id) (renameExp env e) -- id[e]
renameExp env (ENeg e)        = ENeg $ renameExp env e
renameExp env (ENot e)        = ENot $ renameExp env e
renameExp env (EMul e1 op e2) = EMul (renameExp env e1) op (renameExp env e2)
renameExp env (EAdd e1 op e2) = EAdd (renameExp env e1) op (renameExp env e2)
renameExp env (ERel e1 op e2) = ERel (renameExp env e1) op (renameExp env e2)
renameExp env (EAnd e1 e2)    = EAnd (renameExp env e1) (renameExp env e2)
renameExp env (EOr e1 e2)     = EOr (renameExp env e1) (renameExp env e2)
renameExp env (EType t e)     = EType t $ renameExp env e


