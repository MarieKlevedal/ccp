module AlphaRen where

import AbsJavalette

import Data.Map
import qualified Data.Map as M

data Env = Env {
    funcs  :: FuncSig,
    cxts   :: [Cxt],
    fCount :: Int,
    vCount :: Int
}                  
type FuncSig = Map Ident Ident
type Cxt = Map Ident Ident

-------------------------------------------------------------

addVToEnv :: Env -> Ident -> (Env, Ident)
addVToEnv env id = (env'', newId) where
    env'   = env{vCount = (vCount env)+1}
    newId  = newVarId $ vCount env'
    (c:cs) = cxts env'
    c'     = insert id newId c
    env''  = env'{cxts = (c':cs)}

newVarId :: Int -> Ident
newVarId n = Ident $ "v" ++ show n

newFuncId :: Int -> Ident
newFuncId n = Ident $ "f" ++ show n

startEnv :: Env
startEnv = Env {
    funcs  = M.empty,
    cxts   = [],
    fCount = 0,
    vCount = 0
} 
