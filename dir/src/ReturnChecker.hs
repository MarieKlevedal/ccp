module ReturnChecker where

import AbsJavalette
import PrintJavalette
import ErrM

-- typecheck first adds all the definitions of the program to the environment and then
-- makes sure all definitions are type correct
returncheck :: Program -> Err Program
returncheck p@(PProg ds) = do
    let main = ((Ident "main"), (TFun TInt []))
    let sigs = Prelude.map getFuncSig ds
    if elem main sigs then do
        checkDefs ds 
        Ok p
    else Bad "There is no correctly typed main method in the program"
    
getFuncSig :: Def -> (Ident, Type)
getFuncSig d@(FnDef rt id ats _) = (id, (TFun rt (Prelude.map (\(DArg t id) -> t) ats)))

-- checkDefs calls checkDef for all defs in the program
checkDefs :: [Def] -> Err ()
checkDefs [] = Ok ()
checkDefs (d:ds) = do
    checkDef d
    checkDefs ds
    Ok ()


checkDef :: Def -> Err ()
checkDef (FnDef TVoid _            _ _            ) = Ok ()
checkDef (FnDef _     (Ident name) _ (DBlock stms)) = case checkStms stms of
    Ok ()   -> Ok ()
    Bad str -> Bad $ "Function " ++ name ++ str


checkStms :: [Stm] -> Err ()
checkStms []     = Bad " doesn't have a correct return statement"
checkStms (s:ss) = case s of
    SVRet           -> Bad " not allowed to return void"
    SRet    _       -> Ok ()
    SIf     e s1    -> case e of
        ELit LTrue      -> checkStms (s1:ss)
        _               -> checkStms ss
    SIfElse e s1 s2 -> case e of
        ELit LTrue      -> checkStms (s1:ss)
        ELit LFalse     -> checkStms (s2:ss)
        _               -> checkStms ss
    SWhile  e s1    -> case e of
        ELit LTrue      -> checkStms (s1:ss)
        _               -> checkStms ss
    _               -> checkStms ss

