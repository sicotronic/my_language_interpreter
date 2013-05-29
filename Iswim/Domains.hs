module Iswim.Domains where

data Value = Lint Vint | Lbool Vbool | Lfun Vfun | Ltuple Vtuple | Lref Vref

type Vint = Integer
type Vbool = Bool
type Vfun = Sigma -> Value -> Dom
type Vtuple = [Value]
type Vref = Int 

data Dom = Lnorm Sigma Value | Error String Sigma | TypeError | Lout Vint Dom | Lin (Vint -> Dom)

divby0 :: Sigma -> Dom
divby0 = Error "div by 0"

outofrange :: Sigma -> Dom
outofrange = Error "out of range"

star :: (Sigma -> Value -> Dom) -> Dom -> Dom
star h (Lnorm s v) = h s v
star h (Lout i d) = Lout i (star h d)
star h (Lin f) = Lin (\i -> star h (f i))
star h err = err

box :: (String -> Sigma -> Dom) -> Dom -> Dom
box h (Error str s) = h str s
box h (Lout i d) = Lout i (box h d)
box h (Lin f) = Lin (\i -> box h (f i))
box h normotyerr = normotyerr

fun :: (Sigma -> Vfun -> Dom) -> Sigma -> Value -> Dom
fun h s (Lfun f) = h s f
fun h s notafun = TypeError 

funS :: (Sigma -> Vfun -> Dom) -> Dom -> Dom
funS = star . fun

int :: (Sigma -> Vint -> Dom) -> Sigma -> Value -> Dom
int h s (Lint i) = h s i                        
int h s notanint = TypeError 

intS :: (Sigma -> Vint -> Dom) -> Dom -> Dom
intS = star . int

bool :: (Sigma -> Vbool -> Dom) -> Sigma -> Value -> Dom
bool h s (Lbool b) = h s b                        
bool h s notabool = TypeError 

boolS :: (Sigma -> Vbool -> Dom) -> Dom -> Dom
boolS = star . bool

tuple :: (Sigma -> Vtuple -> Dom) -> Sigma -> Value -> Dom
tuple h s (Ltuple t) = h s t                        
tuple h s notatuple = TypeError 

tupleS :: (Sigma -> Vtuple -> Dom) -> Dom -> Dom
tupleS = star . tuple

ref :: (Sigma -> Vref -> Dom) -> Sigma -> Value -> Dom
ref h s (Lref r) = h s r                        
ref h s notaref = TypeError 

refS :: (Sigma -> Vref -> Dom) -> Dom -> Dom
refS = star . ref

type Env = [(String,Value)]
type Sigma = [(Vref,Value)]

on :: Eq a => [(a,b)] -> a -> b
on [] x = error "not in env or state"
on ((a,b):ps) x | x == a = b
                | otherwise = ps `on` x

upd :: [(a,b)] -> a -> b -> [(a,b)]
upd ps a b = (a,b):ps

new :: Sigma -> Vref
new = length

sinit :: Sigma
sinit = []

ninit :: Env
ninit = []

