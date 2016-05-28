module LLVM where

import AbsJavalette

data HeaderInstr =
      FuncDecl Type String [Type]
    | GlobStr String Int String
    | CreateArr String String Type
    | Empty

data Instruction =
      Text String
    | GEP_String String Int String
    | GEP_Size String Type String
    | GEP_Length String Type String
    | GEP_Index String Type String String
    | CallBitCast String String Type
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

instance Show HeaderInstr where
    show (FuncDecl t name ts)     = "declare " ++ showType t ++ " " ++ name ++
                                    "(" ++ showTypes ts ++ ")"
    show (GlobStr name len s)     = name ++ " = internal constant [" ++ show len ++
                                    " x i8] c\"" ++ s ++ "\00\""
    show (CreateArr arr sarr t)   = arr ++ " = type " ++ sarr ++ "*\n" ++ 
                                    sarr ++ " = type {i32, [0 x " ++ showType t ++
                                    "]}"  
    show  Empty                   = ""

instance Show Instruction where
    show (Text s)                 = s
    show (GEP_String lId len gId) = lId ++ " = getelementptr [" ++ show len ++
                                    " x i8]* " ++ gId ++ ", i32 0, i32 0"
    show (GEP_Size lId1 t lId2)   = lId1 ++ " = getelementptr " ++ showType t ++ 
                                    "* null, i32 1\n" ++ lId2 ++ " = ptrtoint " ++
                                    showType t ++ "* " ++ lId1 ++ " to i32"
    show (GEP_Length ret t arr)   = ret ++ " = getelementptr " ++ showArrType t ++
                                    " " ++ arr ++ ", i32 0, i32 0" 
    show (GEP_Index ret t arr ix) = ret ++ " = getelementptr " ++ showArrType t ++
                                    " " ++ arr ++ ", i32 0, i32 1, i32 " ++ ix 
    show (CallBitCast ret id t)   = ret ++ " = bitcast i8* " ++ id ++ " to " ++
                                    showArrType t
    show (VFuncCall t id args)    = "call " ++ showType t ++ " " ++ id ++ "(" ++ 
                                    showArgs args ++ ")"
    show (FuncCall ret t id args) = ret ++ " = call " ++ showType t ++ " " ++ id ++ 
                                    "(" ++ showArgs args ++ ")"
    show (Label s)                = "\n" ++ s ++ ": "
    show (Br1 str)                = "br label %" ++ str
    show (Br2 e l1 l2)            = "br i1 " ++ e ++ ", label %" ++ l1 ++ 
                                    ", label %" ++ l2
    show (Alloca t id)            = id ++ " = alloca " ++ showType t 
    show (Store t lId jId)        = "store " ++ showType t ++ " " ++ lId ++ 
                                    " , " ++ showType t ++ "* " ++ jId
    show (Load lId t jId)         = lId ++ " = load " ++ showType t ++ "* " ++ jId
    show (Return t s)             = "ret " ++ showType t ++ " " ++ s    
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
    show (BCmp res op id1 id2)    = res ++ " = icmp " ++ showIRelOp op ++ " i1 " ++
                                    id1 ++ ", " ++ id2
    show (ICmp res op id1 id2)    = res ++ " = icmp " ++ showIRelOp op ++ " i32 " ++
                                    id1 ++ ", " ++ id2
    show (FCmp res op id1 id2)    = res ++ " = fcmp " ++ showFRelOp op ++ " double " ++
                                    id1 ++ ", " ++ id2

                                    
---------------------- Auxiliary show functions --------------------------------
                                    
showType :: Type -> String
showType t = case t of
    TInt    -> "i32"
    TDoub   -> "double"
    TBool   -> "i1"
    TStr    -> "i8*"
    TVoid   -> "void"
    TArr TInt -> "%intArr"
    TArr TDoub -> "%doubArr"
    TArr TBool -> "%boolArr"

showTypes :: [Type] -> String
showTypes []     = ""
showTypes [t]    = showType t
showTypes (t:ts) = showType t ++ ", " ++ showTypes ts 

showArrType :: Type -> String
showArrType TInt  = "%intArr"
showArrType TDoub = "%doubArr"
showArrType TBool = "%boolArr"

showArgs :: [(Type, String)] -> String
showArgs []            = ""
showArgs [(t, s)]      = showType t ++ " " ++ s
showArgs ((t, s):args) = showType t ++ " " ++ s ++ ", " ++ showArgs args

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

