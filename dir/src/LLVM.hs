module LLVM where

import AbsJavalette


data Instruction =
    Comment String     |
    Text String        |
    Label String       |
    Iconst1            |
    Iconst0            |
    ILoad Int          |   --address
    IStore Int         |   --address
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

-------------------------- ******************** -------------------------
-------------------------- Pretty printer funcs -------------------------
-------------------------- ******************** -------------------------

-- TODO: remane to "instance show where" stuff

-- instrToStr converts instructions to llvm command strings
instrToStr :: Instruction -> String
instrToStr (Comment str)      = "; " ++ str
instrToStr (Text str)         = str
instrToStr (Label str)        = str ++ ":"
instrToStr Iconst1            = "iconst_1"
instrToStr Iconst0            = "iconst_0"
instrToStr (Bipush x)         = "bipush " ++ (show x)
instrToStr Pop                = "pop"
instrToStr Duplicate          = "dup"
instrToStr (ILoad addr)       = "iload " ++ (show addr)
instrToStr (IStore addr)      = "istore " ++ (show addr)
instrToStr IMul               = "imul"
instrToStr IDiv               = "idiv"
instrToStr IAdd               = "iadd"
instrToStr ISub               = "isub"
instrToStr (IfCmLT label)     = "if_icmplt " ++ label
instrToStr (IfCmLTEQ label)   = "if_icmple " ++ label
instrToStr (IfCmEQ label)     = "if_icmpeq " ++ label
instrToStr (IfCmNEQ label)    = "if_icmpne " ++ label
instrToStr (IfEQ label)       = "ifeq " ++ label
instrToStr (Goto label)       = "goto " ++ label
instrToStr Return             = "return"
instrToStr IReturn            = "ireturn"
instrToStr (Invoke c f t)     = "invokestatic "


-- typeToStr converts types to llvm types
typeToStr :: Type -> String
typeToStr t = case t of
    TInt    -> "i32"
    TDoub   -> "double"
    TBool   -> "i1"
    TStr -> "i8*"
    TVoid   -> "void"
