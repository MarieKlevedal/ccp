module LLVM where

import AbsJavalette


data Instruction =
    --Comment String     |
      Text  String
    | Label String
    | Alloca Type String
    | Store Type String String
    | Load String Type String
    | Return Type String
    | IAdd String String String
   {-
    Iconst1            |
    Iconst0            |
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
-}
-------------------------- ******************** -------------------------
-------------------------- Pretty printer funcs -------------------------
-------------------------- ******************** -------------------------

-- TODO: remane to "instance show where" stuff

instance Show Instruction where
    show (Text s)             = s
    show (Label s)            = s ++ ": "
    show (Alloca t id)        = id ++ " = alloca " ++ toLType t 
    show (Store t lId jId)    = "store " ++ (toLType t) ++ " " ++ lId ++ 
                                " , " ++ (toLType t) ++ "* " ++ jId
    show (Load lId t jId)     = lId ++ " = load " ++ (toLType t) ++ "* " ++ jId
    show (Return t s)         = "ret " ++ (toLType t) ++ " " ++ s         
    show (IAdd res id1 id2)   = res ++ " = add i32 " ++ id1 ++ " , " ++ id2
    
    show i                    = "instr"


-- toLType converts types to llvm types
toLType :: Type -> String
toLType t = case t of
    TInt    -> "i32"
    TDoub   -> "double"
    TBool   -> "i1"
    TStr    -> "i8*"
    TVoid   -> "void"

