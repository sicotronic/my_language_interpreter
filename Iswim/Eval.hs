module Iswim.Eval where

import Iswim.Iswim
import Iswim.Domains

eval :: Exp -> Env -> Sigma -> Dom
eval (Var v) n s = Lnorm s (n `on` v)
eval (App e0 e1) n s = (\s0 f -> (\s1 z -> f s1 z) `star` eval e1 n s0) `funS` eval e0 n s  --  modified
eval (Lam v e) n s = Lnorm s (Lfun (\s0 z -> eval e (upd n v z) s0))
eval (Nat i) n s = Lnorm s (Lint i)
eval (Bool b) n s = Lnorm s (Lbool b)
eval (Opp e) n s = (\s0 i -> Lnorm s0 (Lint $ -i)) `intS` eval e n s
eval (Not e) n s = (\s0 b -> Lnorm s0 (Lbool $ not b)) `boolS` eval e n s
eval (Bin e0 op e1) n s 
   | op `elem` [Plus, Minus, Times] = (\s0 i0 -> (\s1 i1 -> Lnorm s1 (Lint $ aop op i0 i1)) `intS` eval e1 n s0) `intS` eval e0 n s
   | op `elem` [Div, Mod] = (\s0 i0 -> (\s1 i1 -> if i1 == 0 then divby0 s1 else Lnorm s1 (Lint $ aop op i0 i1)) `intS` eval e1 n s0) `intS` eval e0 n s
   | op `elem` [Equ, NEqu, Gt, Gte, Lt, Lte] = (\s0 i0 -> (\s1 i1 -> Lnorm s1 (Lbool $ rop op i0 i1)) `intS` eval e1 n s0) `intS` eval e0 n s
   | op `elem` [And, Or, Impl, Iff] = (\s0 b0 -> (\s1 b1 -> Lnorm s1 (Lbool $ bop op b0 b1)) `boolS` eval e1 n s0) `boolS` eval e0 n s
eval (If e0 e1 e2) n s = (\s0 b0 -> if b0 then eval e1 n s0 else eval e2 n s0) `boolS` eval e0 n s
eval (Tuple []) n s = Lnorm s (Ltuple [])
eval (Tuple (e:es)) n s = (\s0 z -> (\s1 zs -> Lnorm s1 (Ltuple $ z:zs)) `tupleS` eval (Tuple es) n s0) `star` eval e n s
eval (Select e k) n s = (\s0 t -> if k < length t then Lnorm s0 (t!!k) else outofrange s0) `tupleS` eval e n s
eval (Ref e) n s = (\s0 z -> lnormRef s0 z) `star` eval e n s -- modified
eval (Val e) n s = (\s0 r -> Lnorm s0 (s0 `on` r)) `refS` eval e n s
eval (Asign e0 e1) n s = (\s0 r -> (\s1 z -> Lnorm (upd s1 r z) z) `star` eval e1 n s0) `refS` eval e0 n s -- modified
eval (Equal e0 e1) n s = (\s0 r0 -> (\s1 r1 -> Lnorm s1 (Lbool (r0 == r1))) `refS` eval e1 n s0) `refS` eval e0 n s -- modified
eval In n s = Lin (\i -> Lnorm s (Lint i))
eval (Out e) n s = (\s0 i -> Lout i (Lnorm s0 (Lint i))) `intS` eval e n s
eval (Fail l) n s = Error l s
eval (Catch l e0 e1) n s = (\str s0 -> if l == str then eval e1 n s0 else Error str s0) `box` eval e0 n s
eval (Letrec w v e0 e1) n s = eval e1 (upd n w (Lfun f)) s
  where 
    f :: Vfun
    f = fix g
    g :: Vfun -> Vfun
    g h s0 z = eval e0 (upd (upd n w (Lfun h)) v z) s0 

fix :: (a -> a) -> a
fix f = f (fix f)

lnormRef :: Sigma -> Value -> Dom -- new function to make the new reference within the same context
lnormRef s z = Lnorm (upd s r z) (Lref r) where r = new s

