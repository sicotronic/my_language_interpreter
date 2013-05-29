-- Translation from the syntax of my imperative language to the Iswim syntax
module Iswim.Translate where
import Iswim.Iswim
import Imp.Syntax

translate :: Comm -> Exp
translate (Skip) = skip 
translate (v := e) = Asign (Iswim.Iswim.Var v) (translate_int e) -- this function only sees the variable to the right
translate (Output v) = Out (translate_int (Imp.Syntax.Var v))
translate (Input v) = Asign (Iswim.Iswim.Var v) In
translate (Imp.Syntax.If b c0 c1) = Iswim.Iswim.If (translate_bool b) (translate c0) (translate c1)
translate (While e0 e1) = while (translate_bool e0) (translate e1)
translate (Seq cmds) = seq_trans(cmds)
translate (For v e0 e1 c0) = for v (translate_int e0) (translate_int e1) (translate c0)
translate (Newvar v e0 c) = newvar v (translate_int e0) (translate c)
translate (Imp.Syntax.Fail l) = Iswim.Iswim.Fail l
translate (Imp.Syntax.Catch l c0 c1) = Iswim.Iswim.Catch l (translate c0) (translate c1)

translate_bool :: BoolExp -> Exp
translate_bool (BoolCte b) = Bool b
translate_bool (Imp.Syntax.Not b) = Iswim.Iswim.Not (translate_bool b)
translate_bool (Imp.Syntax.And b0 b1) = Bin (translate_bool b0) Iswim.Iswim.And (translate_bool b1)
translate_bool (Imp.Syntax.Or b0 b1) = Bin (translate_bool b0) Iswim.Iswim.Or (translate_bool b1)
translate_bool (Imp.Syntax.Impl b0 b1) = Bin (translate_bool b0) Iswim.Iswim.Impl (translate_bool b1)
translate_bool (Equiv b0 b1) = Bin (translate_bool b0) Iff (translate_bool b1)
translate_bool (GrTh e0 e1) = Bin (translate_int e0) Gt (translate_int e1)
translate_bool (LsTh e0 e1) = Bin (translate_int e0) Lt (translate_int e1)
translate_bool (GrThEq e0 e1) = Bin (translate_int e0) Gte (translate_int e1)
translate_bool (LsThEq e0 e1) = Bin (translate_int e0) Lte (translate_int e1)
translate_bool (Dis e0 e1) = Bin (translate_int e0) NEqu (translate_int e1)
translate_bool (Eq e0 e1) = Bin (translate_int e0) Equ (translate_int e1)

translate_int :: IntExp -> Exp
translate_int (Imp.Syntax.Var v) = Val (Iswim.Iswim.Var v)
translate_int (Intr e) = Nat e
translate_int (Op e) = Opp (translate_int e)
translate_int (Mult e0 e1) = Bin (translate_int e0) Times (translate_int e1)
translate_int (Imp.Syntax.Div e0 e1) = Bin (translate_int e0) Iswim.Iswim.Div (translate_int e1)
translate_int (Imp.Syntax.Mod e0 e1) = Bin (translate_int e0) Iswim.Iswim.Mod (translate_int e1)
translate_int (Sub e0 e1) = Bin (translate_int e0) Minus (translate_int e1)
translate_int (Ad e0 e1) = Bin (translate_int e0) Plus (translate_int e1)

seq_trans :: [Comm] -> Exp
seq_trans cmds = if (null cmds) then skip else App (Lam (fresh (list_fv cmds)) (seq_trans (tail cmds)) ) (translate (head cmds))

while :: Exp -> Exp -> Exp  -- while b do c = letrec w = Lam v if b then c; w <> else skip in w <> where w is not free in b o c
while b c = Letrec w v e0 e1
		where e0 = letin w (Lam v (Iswim.Iswim.If b (letin v' c e1) skip)) e1
		      e1 = (App (Iswim.Iswim.Var w) skip)
		      w = fresh (fv_exp c ++ fv_exp b)
		      v = fresh ((fv_exp c ++ fv_exp b)++[w])
		      v' = fresh (fv_exp c ++ fv_exp b ++ [v,w])

for :: String -> Exp -> Exp -> Exp -> Exp
for v e0 e1 c0 = newvar v e0 (newvar w e1 (while b c))
		  where b = Bin (Val (Iswim.Iswim.Var v)) Lte (Val (Iswim.Iswim.Var w))
			c = letin v' c0 (Asign (Iswim.Iswim.Var v) (Bin (Val (Iswim.Iswim.Var v)) Plus (Nat 1)))
			w = fresh (fv_exp e0 ++ fv_exp e1 ++ fv_exp c0)
			v' = fresh (fv_exp e0 ++ fv_exp e1 ++ fv_exp c0 ++ [v,w])

newvar :: String -> Exp -> Exp -> Exp
newvar v e c = letin v (Ref e) c

letin :: String -> Exp -> Exp -> Exp  ---let v = e in e' ==> (\v e') e
letin v e0 e1 = App (Lam v e1) e0

skip :: Exp -- the semantic for skip is the same as the empty tuple
skip = Tuple []

fresh :: [String] -> String
fresh = head . diff vars

diff :: Eq a => [a] -> [a] -> [a] -- it returns a set with all the elements in xs that are not in ys
diff [] ys = []
diff (x:xs) ys = if (notElem x ys) then x:(diff xs ys) else diff xs ys

vars = ["x" ++ show i | i <- [1..]] -- description of the variables by comprehension

fv_exp :: Exp -> [String]
fv_exp (Iswim.Iswim.Var v) = [v]
fv_exp (App e0 e1) = fv_exp e0 ++ fv_exp e1
fv_exp (Lam v e) = [v] ++ fv_exp e
fv_exp (Nat i) = []
fv_exp (Bool b) = []
fv_exp (Opp i) = []
fv_exp (Iswim.Iswim.Not b) = []
fv_exp (Bin e0 Plus e1) = fv_exp e0 ++ fv_exp e1
fv_exp (Bin e0 Minus e1) = fv_exp e0 ++ fv_exp e1
fv_exp (Bin e0 Times e1) = fv_exp e0 ++ fv_exp e1
fv_exp (Bin e0 Iswim.Iswim.Mod e1) = fv_exp e0 ++ fv_exp e1
fv_exp (Bin e0 Equ e1) = fv_exp e0 ++ fv_exp e1
fv_exp (Bin e0 Iswim.Iswim.Div e1) = fv_exp e0 ++ fv_exp e1
fv_exp (Bin e0 NEqu e1) = fv_exp e0 ++ fv_exp e1
fv_exp (Bin e0 Gt e1) = fv_exp e0 ++ fv_exp e1
fv_exp (Bin e0 Gte e1) = fv_exp e0 ++ fv_exp e1
fv_exp (Bin e0 Lt e1) = fv_exp e0 ++ fv_exp e1
fv_exp (Bin e0 Lte e1) = fv_exp e0 ++ fv_exp e1
fv_exp (Bin e0 Iswim.Iswim.And e1) = fv_exp e0 ++ fv_exp e1
fv_exp (Bin e0 Iswim.Iswim.Or e1) = fv_exp e0 ++ fv_exp e1
fv_exp (Bin e0 Iswim.Iswim.Impl e1) = fv_exp e0 ++ fv_exp e1
fv_exp (Bin e0 Iff e1) = fv_exp e0 ++ fv_exp e1
fv_exp (Iswim.Iswim.If b e0 e1) = fv_exp b ++ fv_exp e0 ++ fv_exp e1
fv_exp (Tuple [e]) = fv_exp e
fv_exp (Tuple []) = []
fv_exp (Select e i) = fv_exp e
fv_exp (Letrec w v e0 e1) = [w,v] ++ fv_exp e0 ++ fv_exp e1
fv_exp (Ref e) = fv_exp e
fv_exp (Val e) = fv_exp e
fv_exp (Asign e0 e1) = fv_exp e0 ++ fv_exp e1
fv_exp (Equal e0 e1) = fv_exp e0 ++ fv_exp e1
fv_exp (In) = []
fv_exp (Out e) = fv_exp e 
fv_exp (Iswim.Iswim.Fail l) = []
fv_exp (Iswim.Iswim.Catch v e0 e1) = fv_exp e0 ++ fv_exp e1

list_fv :: [Comm] -> [String]
list_fv [] = []
list_fv (c0:cs) = fv_exp (translate (c0)) ++ list_fv cs

