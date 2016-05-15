{-# LANGUAGE FlexibleInstances #-}

data Term = T
		  | F
		  | Var String
		  | Not Term 
		  | Or Term Term 
		  | And Term Term 
		  | Impl Term Term 
		  | Iff Term Term 
		  | Niff Term Term


(\/) :: Term -> Term -> Term
(\/) t1 t2 = Or t1 t2

(/\) :: Term -> Term -> Term
(/\) t1 t2 = And t1 t2

(==>) :: Term -> Term -> Term
(==>) t1 t2 = Impl t1 t2

(<==>) :: Term -> Term -> Term
(<==>) t1 t2 = Iff t1 t2

(!<==>) :: Term -> Term -> Term
(!<==>) t1 t2 = Niff t1 t2

showTerm :: Term -> String

showTerm (Var i) = id i

showTerm (Not (Var i)) = "!" ++ showTerm t
showTerm (Not T) = "!" ++ showTerm T
showTerm (Not F) = "!" ++ showTerm F
showTerm (Not t) = "!(" ++ showTerm t ++ ")"

showTerm T = "true"
showTerm F = "false"

showTerm (Or (Var i) (Var j)) = showTerm(Var i) ++ " \\/ " ++ showTerm(Var j)
showTerm (Or (Var i) t) = showTerm(Var i) ++ " \\/ (" ++ showTerm(t) ++ ")"
showTerm (Or t (Var i)) = "(" ++ showTerm(t) ++ ")" ++ " \\/ " ++ showTerm(Var i)
showTerm (Or t1 t2) = "(" ++ showTerm t1 ++ ") \\/ (" ++ showTerm t2 ++ ")"

showTerm (And (Var i) (Var j)) = showTerm(Var i) ++ " /\\ " ++ showTerm(Var j)
showTerm (And (Var i) t) = showTerm(Var i) ++ " /\\ (" ++ showTerm(t) ++ ")"
showTerm (And t (Var i)) = "(" ++ showTerm(t) ++ ")" ++ " /\\ " ++ showTerm(Var i)
showTerm (And t1 t2) = "(" ++ showTerm t1 ++ ") /\\ (" ++ showTerm t2 ++ ")"

showTerm (Impl (Var i) (Var j)) = showTerm(Var i) ++ " ==> " ++ showTerm(Var j)
showTerm (Impl (Var i) t) = showTerm(Var i) ++ " ==> (" ++ showTerm(t) ++ ")"
showTerm (Impl t (Var i)) = "(" ++ showTerm(t) ++ ")" ++ " ==> " ++ showTerm(Var i)
showTerm (Impl t1 t2) = "(" ++ showTerm t1 ++ ") ==> (" ++ showTerm t2 ++ ")"

showTerm (Iff (Var i) (Var j)) = showTerm(Var i) ++ " <==> " ++ showTerm(Var j)
showTerm (Iff (Var i) t) = showTerm(Var i) ++ " <==> (" ++ showTerm(t) ++ ")"
showTerm (Iff t (Var i)) = "(" ++ showTerm(t) ++ ")" ++ " <==> " ++ showTerm(Var i)
showTerm (Iff t1 t2) = "(" ++ showTerm t1 ++ ") <==> (" ++ showTerm t2 ++ ")"

showTerm (Niff (Var i) (Var j)) = showTerm(Var i) ++ " !<==> " ++ showTerm(Var j)
showTerm (Niff (Var i) t) = showTerm(Var i) ++ " !<==> (" ++ showTerm(t) ++ ")"
showTerm (Niff t (Var i)) = "(" ++ showTerm(t) ++ ")" ++ " !<==> " ++ showTerm(Var i)
showTerm (Niff t1 t2) = "(" ++ showTerm t1 ++ ") !<==> (" ++ showTerm t2 ++ ")"

instance Show Term where show = showTerm

a :: Term
a = Var "a"

b :: Term
b = Var "b"

c :: Term
c = Var "c"

d :: Term
d = Var "d"

e :: Term
e = Var "e"

f :: Term
f = Var "f"

g :: Term
g = Var "g"

h :: Term
h = Var "h"

i :: Term
i = Var "i"

j :: Term
j = Var "j"

k :: Term
k = Var "k"

l :: Term
l = Var "l"

m :: Term
m = Var "m"

n :: Term
n = Var "n"

o :: Term
o = Var "o"

p :: Term
p = Var "p"

q :: Term
q = Var "q"

r :: Term
r = Var "r"

s :: Term
s = Var "s"

t :: Term
t = Var "t"

u :: Term
u = Var "u"

v :: Term
v = Var "v"

w :: Term
w = Var "w"

x :: Term
x = Var "x"

y :: Term
y = Var "y"

z :: Term
z = Var "z"

data Equation = Equation Term Term

instance Eq Term where
	Var t1 == Var t2 = t1 == t2
	T == T = True
	F == F = True
	(Or t1 t2) == (Or t3 t4) = (t1 == t3) && (t2 == t4)
	(And t1 t2) == (And t3 t4) = (t1 == t3) && (t2 == t4)
	(Impl t1 t2) == (Impl t3 t4) = (t1 == t3) && (t2 == t4)
	(Iff t1 t2) == (Iff t3 t4) = (t1 == t3) && (t2 == t4)
	(Niff t1 t2) == (Niff t3 t4) = (t1 == t3) && (t2 == t4)
	(Not t1) == (Not t2) = t1 == t2
	_ == _ = False

instance Eq Equation where
	Equation t1 t2 == Equation t3 t4
		| t1 == t3 && t2 == t4 = True
		| t1 == t4 && t2 == t3 = True
		| otherwise = False

instance Show Equation where
	show (Equation t1 t2) = show t1 ++ " === " ++ show t2

-- Asumimos que la entrada es correcta
type Sust = (Term, Term)

-- Abstraer de Flaviani

ci = \x -> x
ck = \x -> \y -> x
cs = \x -> \y -> \z -> (x z) (y z)

abstraer :: Term -> Term -> (Term -> Term)
abstraer (Var x) (Var y) = if x == y then ci else ck (Var y)
abstraer (Var x) (Or t1 t2) = cs (cs (ck Or) (abstraer (Var x) t1)) (abstraer (Var x) t2)
abstraer (Var x) (And t1 t2) = cs (cs (ck And) (abstraer (Var x) t1)) (abstraer (Var x) t2)
abstraer (Var x) (Impl t1 t2) = cs (cs (ck Impl) (abstraer (Var x) t1)) (abstraer (Var x) t2)
abstraer (Var x) (Iff t1 t2) = cs (cs (ck Iff) (abstraer (Var x) t1)) (abstraer (Var x) t2)
abstraer (Var x) (Niff t1 t2) = cs (cs (ck Niff) (abstraer (Var x) t1)) (abstraer (Var x) t2)

abstraer (Or t1 t2) _ = error "Solo se puede abstraer una variable"


class Instantiable s where
	sustituir :: Term -> s -> Term
	sustVar :: Term -> s -> Term
	instantiate :: Equation -> s -> Equation
	instantiate (Equation t1 t2) sus = Equation (sustituir t1 sus) (sustituir t2 sus)

	-- Casos recursivos
	sustituir (Or t1 t2) sus = Or (sustituir t1 sus) (sustituir t2 sus)
	sustituir (And t1 t2) sus = And (sustituir t1 sus) (sustituir t2 sus)
	sustituir (Impl t1 t2) sus = Impl (sustituir t1 sus) (sustituir t2 sus)
	sustituir (Iff t1 t2) sus = Iff (sustituir t1 sus) (sustituir t2 sus)
	sustituir (Niff t1 t2) sus = Niff (sustituir t1 sus) (sustituir t2 sus)
	-- Casos base
	sustituir T _ = T
	sustituir F _ = F
	sustituir (Var v1) sus = sustVar (Var v1) sus

instance Instantiable Sust where
	sustVar (Var v1) (sustitucion, variable) = (abstraer variable (Var v1)) sustitucion

instance Instantiable (Term, Sust, Term) where
	sustVar (Var v1) (t1, (t2, s1), s2)
		| (Var v1) == s1 = (abstraer (Var v1) (Var v1)) t1
		| (Var v1) == s2 = (abstraer (Var v1) (Var v1)) t2
		| otherwise = Var v1

instance Instantiable (Term, Term, Sust, Term, Term) where
	sustVar (Var v1) (t1, t2, (t3, s1), s2, s3)
		| (Var v1) == s1 = (abstraer (Var v1) (Var v1)) t1
		| (Var v1) == s2 = (abstraer (Var v1) (Var v1)) t2
		| (Var v1) == s3 = (abstraer (Var v1) (Var v1)) t3
		| otherwise = Var v1

true :: Term
true = T
expr :: Term
expr = p /\ (p ==> q) ==> p

teo33 :: Term
teo33 = true <==> q <==> q

pruebaMonad :: IO Term
pruebaMonad = return true
	>>= \s -> print s 
	>>= \_ -> putStrLn ("<q:= " ++ show expr ++ "en " ++ show teo33 ++ ">")
	>>= \_ -> return (sustituir teo33 (expr,q))
	>>= \s1 -> print s1
	>>= \_ -> putStrLn ("<q:= p en " ++ show s1 ++ ">")
	>>= \_ -> return (sustituir s1 (p,q))
	>>= \s2 -> print s2
	>>= \_ -> putStrLn ("<p:= x en " ++ show s2 ++ ">")
	>>= \_ -> return (sustituir s2 (x, p))
	>>= \s3 -> print s3
	>>= \_ -> return true

