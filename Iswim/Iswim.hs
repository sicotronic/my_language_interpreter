-- Syntax for ISWIM expressions
module Iswim.Iswim where

data Exp = Var String
         | App Exp Exp
         | Lam String Exp
         | Nat Integer
         | Bool Bool
         | Opp Exp
         | Not Exp
         | Bin Exp Oper Exp
         | If Exp Exp Exp
         | Tuple [Exp]
         | Select Exp Int
         | Letrec String String Exp Exp
         | Ref Exp
         | Val Exp
         | Asign Exp Exp
         | Equal Exp Exp
         | In
         | Out Exp
         | Fail String
         | Catch String Exp Exp deriving Show

data Oper = Plus | Minus | Times | Div | Mod | Equ | NEqu | Gt | Gte | Lt | Lte | And | Or | Impl | Iff deriving (Eq, Show)


aop :: Oper -> Integer -> Integer -> Integer
aop Plus = (+)
aop Minus = (-)
aop Times = (*)
aop Div = div
aop Mod = mod
aop _ = error "not arithmetic"

rop :: Oper -> Integer -> Integer -> Bool
rop Equ = (==)
rop NEqu = (/=)
rop Gt = (>)
rop Gte = (>=)
rop Lt = (<)
rop Lte = (<=)
rop _ = error "not relational"

bop :: Oper -> Bool -> Bool -> Bool
bop And = (&&)
bop Or = (||)
bop Impl = (>=)
bop Iff = (==)
bop _ = error "not logical"


