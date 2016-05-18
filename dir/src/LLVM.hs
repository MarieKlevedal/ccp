module LLVM where

import AbsJavalette


data Instruction =
      Text  String
    | GlobStr String Int String
    | GlobArr String Int Type [String]
    | GetElemPtr String Int String
    | VFuncCall Type String [(Type, String)]
    | FuncCall String Type String [(Type, String)]
    | Label String
    | Br1 String
    | Br2 String String String
    | Alloca Type String
    | Store Type String String
    | Load String Type String
    | Return Type String
    | VReturn
    
    | IMul String String String
    | FMul String String String
    | IDiv String String String
    | FDiv String String String
    | IMod String String String
    | IAdd String String String
    | FAdd String String String
    | ISub String String String
    | FSub String String String
    
    | BCmp String RelOp String String
    | ICmp String RelOp String String
    | FCmp String RelOp String String

instance Show Instruction where
    show (Text s)                 = s
    show (GlobStr name len s)     = name ++ " = internal constant [" ++ (show len) ++
                                    " x i8] c\"" ++ s ++ "\00\""
                                    {-
    show (GlobArr name len t els) = name ++ " = {i32, [" ++ show len ++ 
                                    " x " ++ toLType t ++ "] }\n             [" ++ 
                                    showArr t els ++ "]"
                                    -} -- Note: probably incorrect!
    show (GetElemPtr lId len gId) = lId ++ " = getelementptr [" ++ (show len) ++
                                    " x i8]* " ++ gId ++ ", i32 0, i32 0"
    show (VFuncCall t id args)    = "call " ++ (toLType t) ++ " " ++ id ++ "(" ++ 
                                    (showArgs args) ++ ")"
    show (FuncCall ret t id args) = ret ++ " = call " ++ (toLType t) ++ " " ++ id ++ 
                                    "(" ++ (showArgs args) ++ ")"
    show (Label s)                = "\n" ++ s ++ ": "
    show (Br1 str)                = "br label %" ++ str
    show (Br2 e l1 l2)            = "br i1 " ++ e ++ ", label %" ++ l1 ++ 
                                    ", label %" ++ l2
    show (Alloca t id)            = id ++ " = alloca " ++ toLType t 
    show (Store t lId jId)        = "store " ++ (toLType t) ++ " " ++ lId ++ 
                                    " , " ++ (toLType t) ++ "* " ++ jId
    show (Load lId t jId)         = lId ++ " = load " ++ (toLType t) ++ "* " ++ jId
    show (Return t s)             = "ret " ++ (toLType t) ++ " " ++ s    
    show  VReturn                 = "ret void"
    
    -- MulOps
    show (IMul res id1 id2)       = res ++ " = mul i32 " ++ id1 ++ " , " ++ id2     
    show (FMul res id1 id2)       = res ++ " = fmul double " ++ id1 ++ " , " ++ id2
    show (IDiv res id1 id2)       = res ++ " = sdiv i32 " ++ id1 ++ " , " ++ id2     
    show (FDiv res id1 id2)       = res ++ " = fdiv double " ++ id1 ++ " , " ++ id2
    show (IMod res id1 id2)       = res ++ " = srem i32 " ++ id1 ++ " , " ++ id2
    
    -- AddOps
    show (IAdd res id1 id2)       = res ++ " = add i32 " ++ id1 ++ " , " ++ id2
    show (FAdd res id1 id2)       = res ++ " = fadd double " ++ id1 ++ " , " ++ id2
    show (ISub res id1 id2)       = res ++ " = sub i32 " ++ id1 ++ " , " ++ id2
    show (FSub res id1 id2)       = res ++ " = fsub double " ++ id1 ++ " , " ++ id2

    -- RelOps
    show (BCmp res op id1 id2)    = res ++ " = icmp " ++ (showIRelOp op) ++ " i1 " ++
                                    id1 ++ ", " ++ id2
    show (ICmp res op id1 id2)    = res ++ " = icmp " ++ (showIRelOp op) ++ " i32 " ++
                                    id1 ++ ", " ++ id2
    show (FCmp res op id1 id2)    = res ++ " = fcmp " ++ (showFRelOp op) ++ " double " ++
                                    id1 ++ ", " ++ id2

-- toLType converts Javalette types to LLVM types
toLType :: Type -> String
toLType t = case t of
    TInt    -> "i32"
    TDoub   -> "double"
    TBool   -> "i1"
    TStr    -> "i8*"
    TVoid   -> "void"

showArr :: Type -> [String] -> String
showArr t []     = ""
showArr t [s]    = toLType t ++ " " ++ s
showArr t (s:ss) = toLType t ++ " " ++ s ++ ", " ++ showArr t ss

showArgs :: [(Type, String)] -> String
showArgs []            = ""
showArgs [(t, s)]      = (toLType t) ++ " " ++ s
showArgs ((t, s):args) = (toLType t) ++ " " ++ s ++ ", " ++ showArgs args

showIRelOp :: RelOp -> String
showIRelOp Lt   = "slt"
showIRelOp LtEq = "sle"
showIRelOp Gt   = "sgt"
showIRelOp GtEq = "sge"
showIRelOp Eq   = "eq"
showIRelOp NEq  = "ne"

showFRelOp :: RelOp -> String
showFRelOp Lt   = "olt"
showFRelOp LtEq = "ole"
showFRelOp Gt   = "ogt"
showFRelOp GtEq = "oge"
showFRelOp Eq   = "oeq"
showFRelOp NEq  = "one"

