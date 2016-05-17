-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParJavalette where
import AbsJavalette
import LexJavalette
import ErrM

}

%name pProgram Program

-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype { Token }

%token
  '!' { PT _ (TS _ 1) }
  '!=' { PT _ (TS _ 2) }
  '%' { PT _ (TS _ 3) }
  '&&' { PT _ (TS _ 4) }
  '(' { PT _ (TS _ 5) }
  ')' { PT _ (TS _ 6) }
  '*' { PT _ (TS _ 7) }
  '+' { PT _ (TS _ 8) }
  '++' { PT _ (TS _ 9) }
  ',' { PT _ (TS _ 10) }
  '-' { PT _ (TS _ 11) }
  '--' { PT _ (TS _ 12) }
  '.' { PT _ (TS _ 13) }
  '/' { PT _ (TS _ 14) }
  ':' { PT _ (TS _ 15) }
  ';' { PT _ (TS _ 16) }
  '<' { PT _ (TS _ 17) }
  '<=' { PT _ (TS _ 18) }
  '=' { PT _ (TS _ 19) }
  '==' { PT _ (TS _ 20) }
  '>' { PT _ (TS _ 21) }
  '>=' { PT _ (TS _ 22) }
  'String' { PT _ (TS _ 23) }
  '[' { PT _ (TS _ 24) }
  '[]' { PT _ (TS _ 25) }
  ']' { PT _ (TS _ 26) }
  'boolean' { PT _ (TS _ 27) }
  'double' { PT _ (TS _ 28) }
  'else' { PT _ (TS _ 29) }
  'false' { PT _ (TS _ 30) }
  'for' { PT _ (TS _ 31) }
  'if' { PT _ (TS _ 32) }
  'int' { PT _ (TS _ 33) }
  'length' { PT _ (TS _ 34) }
  'new' { PT _ (TS _ 35) }
  'return' { PT _ (TS _ 36) }
  'true' { PT _ (TS _ 37) }
  'void' { PT _ (TS _ 38) }
  'while' { PT _ (TS _ 39) }
  '{' { PT _ (TS _ 40) }
  '||' { PT _ (TS _ 41) }
  '}' { PT _ (TS _ 42) }

L_ident  { PT _ (TV $$) }
L_integ  { PT _ (TI $$) }
L_doubl  { PT _ (TD $$) }
L_quoted { PT _ (TL $$) }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }
Integer :: { Integer } : L_integ  { (read ( $1)) :: Integer }
Double  :: { Double }  : L_doubl  { (read ( $1)) :: Double }
String  :: { String }  : L_quoted {  $1 }

Program :: { Program }
Program : ListDef { PProg $1 } 


ListDef :: { [Def] }
ListDef : Def { (:[]) $1 } 
  | Def ListDef { (:) $1 $2 }


Def :: { Def }
Def : Type Ident '(' ListArg ')' Block { FnDef $1 $2 $4 $6 } 


ListArg :: { [Arg] }
ListArg : {- empty -} { [] } 
  | Arg { (:[]) $1 }
  | Arg ',' ListArg { (:) $1 $3 }


Arg :: { Arg }
Arg : Type Ident { DArg $1 $2 } 


Block :: { Block }
Block : '{' ListStm '}' { DBlock (reverse $2) } 


ListStm :: { [Stm] }
ListStm : {- empty -} { [] } 
  | ListStm Stm { flip (:) $1 $2 }


Stm :: { Stm }
Stm : ';' { SEmpty } 
  | Block { SBlock $1 }
  | Type ListItem ';' { SDecl $1 $2 }
  | Ident '=' Exp ';' { SAss $1 $3 }
  | Ident '[' Exp ']' '=' Exp ';' { SArrAss $1 $3 $6 }
  | Ident '=' 'new' Type '[' Exp ']' ';' { SNewArrAss $1 $4 $6 }
  | Ident '++' ';' { SIncr $1 }
  | Ident '--' ';' { SDecr $1 }
  | 'return' Exp ';' { SRet $2 }
  | 'return' ';' { SVRet }
  | 'if' '(' Exp ')' Stm { SIf $3 $5 }
  | 'if' '(' Exp ')' Stm 'else' Stm { SIfElse $3 $5 $7 }
  | 'while' '(' Exp ')' Stm { SWhile $3 $5 }
  | 'for' '(' Type Ident ':' Exp ')' Stm { SForEach $3 $4 $6 $8 }
  | Exp ';' { SExp $1 }


Item :: { Item }
Item : Ident { IDecl $1 } 
  | Ident '=' Exp { IInit $1 $3 }
  | Ident '=' 'new' Type '[' Exp ']' { IArrInit $1 $4 $6 }


ListItem :: { [Item] }
ListItem : Item { (:[]) $1 } 
  | Item ',' ListItem { (:) $1 $3 }


Type1 :: { Type }
Type1 : 'int' { TInt } 
  | 'double' { TDoub }
  | 'boolean' { TBool }
  | 'void' { TVoid }
  | '(' Type ')' { $2 }


Type :: { Type }
Type : Type1 '[]' { TArr $1 } 
  | Type1 { $1 }


ListType :: { [Type] }
ListType : {- empty -} { [] } 
  | Type { (:[]) $1 }
  | Type ',' ListType { (:) $1 $3 }


Exp6 :: { Exp }
Exp6 : Ident { EVar $1 } 
  | Lit { ELit $1 }
  | Ident '(' ListExp ')' { EApp $1 $3 }
  | Ident '.' 'length' { EArrLen $1 }
  | Ident '[' Exp ']' { EArrInd $1 $3 }
  | '(' Exp ')' { $2 }


Exp5 :: { Exp }
Exp5 : '-' Exp6 { ENeg $2 } 
  | '!' Exp6 { ENot $2 }
  | Exp6 { $1 }


Exp4 :: { Exp }
Exp4 : Exp4 MulOp Exp5 { EMul $1 $2 $3 } 
  | Exp5 { $1 }


Exp3 :: { Exp }
Exp3 : Exp3 AddOp Exp4 { EAdd $1 $2 $3 } 
  | Exp4 { $1 }


Exp2 :: { Exp }
Exp2 : Exp2 RelOp Exp3 { ERel $1 $2 $3 } 
  | Exp3 { $1 }


Exp1 :: { Exp }
Exp1 : Exp2 '&&' Exp1 { EAnd $1 $3 } 
  | Exp2 { $1 }


Exp :: { Exp }
Exp : Exp1 '||' Exp { EOr $1 $3 } 
  | Exp1 { $1 }


ListExp :: { [Exp] }
ListExp : {- empty -} { [] } 
  | Exp { (:[]) $1 }
  | Exp ',' ListExp { (:) $1 $3 }


Lit :: { Lit }
Lit : Integer { LInt $1 } 
  | Double { LDoub $1 }
  | 'true' { LTrue }
  | 'false' { LFalse }
  | String { LStr $1 }


AddOp :: { AddOp }
AddOp : '+' { Plus } 
  | '-' { Minus }


MulOp :: { MulOp }
MulOp : '*' { Times } 
  | '/' { Div }
  | '%' { Mod }


RelOp :: { RelOp }
RelOp : '<' { Lt } 
  | '<=' { LtEq }
  | '>' { Gt }
  | '>=' { GtEq }
  | '==' { Eq }
  | '!=' { NEq }



{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
}

