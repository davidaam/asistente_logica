module Term where

data Term = T
          | F
          | Var String
          | Not Term 
          | Or Term Term 
          | And Term Term 
          | Impl Term Term 
          | Iff Term Term 
          | Niff Term Term

instance Show Term where show = showTerm

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

neg :: Term -> Term
neg t = Not t

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

-- Precedencias y asociatividades

infixl 2 <==>
infixl 2 !<==>
infixr 3 ==>
infixl 4 /\
infixl 4 \/


showTerm :: Term -> String

showTerm (Var i) = i

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

true :: Term
true = T

false :: Term
false = F
