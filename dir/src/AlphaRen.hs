module AlphaRen where

import AbsJavalette

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

-------------------------------------------------------------
startEnv :: Env
startEnv = Env {
    funcs  = M.empty,
    cxts   = [],
    fCount = 0,
    vCount = 0
} 

newCxt  :: Env -> Env
newCxt env = env{cxts = (M.empty):(cxts env)}

addVToEnv :: Env -> Ident -> (Env, Ident)
addVToEnv env id = (env'', newId) where
    env'   = env{vCount = (vCount env)+1}
    newId  = Ident $ "v" ++ show (vCount env')
    (c:cs) = cxts env'
    c'     = M.insert id newId c
    env''  = env'{cxts = (c':cs)}

addFToEnv :: Env -> Ident -> (Env, Ident)
addFToEnv env id@(Ident "main") = (env', id) where
    f      = funcs env'
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
searchCxts (c:cs) id = case M.lookup id c of
    Just newId  -> newId
    Nothing     -> searchCxts cs id

lookupFun :: Env -> Ident -> Ident
lookupFun env id = fromJust $ M.lookup id $ funcs env

----------------------------------------------------------------------------

addFuncs :: Env -> [Def] -> [Def] -> (Env, [Def])
addFuncs env []                          newDs = (env, newDs)
addFuncs env (d@(FnDef t id as b):oldDs) newDs = addFuncs env' oldDs (newDs ++ [d'])
    where (env', newId) = addFToEnv env id 
          d' = (FnDef t newId as b)
          
----------------------------------------------------------------------------

alphaRen :: Program -> Program
alphaRen (PProg ds) = PProg newDs
    where (env, newDs) = addFuncs startEnv ds [] 


--renameDefs :: Env -> [Def] -> [] 



