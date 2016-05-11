module ReturnChecker where

import AbsJavalette
import PrintJavalette
import ErrM

-- returncheck takes a program, checks that it contains a function called
-- main of the correct type and calls checkDefs to check that all functions
-- of the program contain return statements where they should.
returncheck :: Program -> Err Program
returncheck p@(PProg ds) = do
    let main = ((Ident "main"), (TFun TInt []))
    let sigs = Prelude.map getFuncSig ds
    if elem main sigs then do
        checkDefs ds 
        Ok p
    else Bad "There is no correctly typed main method in the program"

-- getFuncSig is an auxuliary function of returncheck that given a
-- definition returns its ident and type.
getFuncSig :: Def -> (Ident, Type)
getFuncSig d@(FnDef rt id ats _) = (id, (TFun rt (Prelude.map (\(DArg t id) -> t) ats)))

-- checkDefs calls checkDef for all Defs (functions) in the program.
checkDefs :: [Def] -> Err ()
checkDefs [] = Ok ()
checkDefs (d:ds) = do
    checkDef d
    checkDefs ds
    Ok ()

-- checkDef takes a Def and calls checkStms for its body if it has an other
-- return type than void.
checkDef :: Def -> Err ()
checkDef (FnDef TVoid _            _ _            ) = Ok ()
checkDef (FnDef _     (Ident name) _ (DBlock stms)) = case checkStms stms of
    Ok ()   -> Ok ()
    Bad str -> Bad $ "Function " ++ name ++ str

-- checkStms checks that a list of statements include at least one return
-- statement. It takes care of branching in If and Where statements.
checkStms :: [Stm] -> Err ()
checkStms []     = Bad " doesn't have a correct return statement"
checkStms (s:ss) = case s of
    SRet    _           -> Ok ()
    SIf     e s1        -> case e of
        EType TBool (ELit LTrue)  -> checkStms (s1:ss)
        _                         -> checkStms ss
    SIfElse e s1 s2     -> case e of
        EType TBool (ELit LTrue)  -> checkStms (s1:ss)
        EType TBool (ELit LFalse) -> checkStms (s2:ss)
        _                         -> do
            checkStms (s1:ss)
            checkStms (s2:ss)
    SWhile  e s1        -> case e of
        EType TBool (ELit LTrue)  -> checkStms [s1] -- modified from (s1:ss)
        _                         -> checkStms ss
    SBlock (DBlock ss1) -> checkStms (ss1 ++ ss)
    _                   -> checkStms ss


