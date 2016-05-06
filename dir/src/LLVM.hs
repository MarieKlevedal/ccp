module LLVM where

import AbsJavalette


data Instruction =
    --Comment String     |
      Text  String
    | GlobStr String Int String
    | GetElemPtr String Int String
    | VFuncCall Type String [(Type, String)]
    | FuncCall String Type String [(Type, String)]
    | Label String
    | Alloca Type String
    | Store Type String String
    | Load String Type String
    | Return Type String
    | IAdd String String String
    | FAdd String String String


instance Show Instruction where
    show (Text s)                 = s
    show (GlobStr name len s)     = name ++ " = internal constant [" ++ (show len) ++
                                    " x i8] c\"" ++ s ++ "\0A\00\""
    show (GetElemPtr lId len gId) = lId ++ " = getelementptr [" ++ (show len) ++
                                    " x i8]* " ++ gId ++ ", i32 0, i32 0"
    show (VFuncCall t id args)    = "call " ++ (toLType t) ++ " " ++ id ++ "(" ++ 
                                    (showArgs args) ++ ")"
    show (FuncCall ret t id args) = ret ++ " = call " ++ (toLType t) ++ " " ++ id ++ 
                                    "(" ++ (showArgs args) ++ ")"
    show (Label s)                = s ++ ": "
    show (Alloca t id)            = id ++ " = alloca " ++ toLType t 
    show (Store t lId jId)        = "store " ++ (toLType t) ++ " " ++ lId ++ 
                                    " , " ++ (toLType t) ++ "* " ++ jId
    show (Load lId t jId)         = lId ++ " = load " ++ (toLType t) ++ "* " ++ jId
    show (Return t s)             = "ret " ++ (toLType t) ++ " " ++ s         
    show (IAdd res id1 id2)       = res ++ " = add i32 " ++ id1 ++ " , " ++ id2
    show (FAdd res id1 id2)       = res ++ " = fadd double " ++ id1 ++ " , " ++ id2


-- toLType converts Javalette types to LLVM types
toLType :: Type -> String
toLType t = case t of
    TInt    -> "i32"
    TDoub   -> "double"
    TBool   -> "i1"
    TStr    -> "i8*"
    TVoid   -> "void"

showArgs :: [(Type, String)] -> String
showArgs []            = ""
showArgs [(t, s)]      = (toLType t) ++ " " ++ s
showArgs ((t, s):args) = (toLType t) ++ " " ++ s ++ ", "
