module Term where

data Operator = Or | And | Impl | Iff | Niff

instance Show Operator where
  show Or = "\\/"
  show And = "/\\"
  show Impl = "==>"
  show Iff = "<==>"
  show Niff = "!<==>"

data Term = T
          | F
          | Var String
          | Not Term
          | Operation Operator Term Term

instance Show Term where show = showTerm

instance Eq Term where
    Var t1 == Var t2 = t1 == t2
    T == T = True
    F == F = True
    (Operation operador1 t1 t2) == (Operation operador2 t3 t4) = (t1 == t3) && (t2 == t4)
    _ == _ = False

neg :: Term -> Term
neg t = Not t

(\/) :: Term -> Term -> Term
(\/) t1 t2 = Operation Or t1 t2

(/\) :: Term -> Term -> Term
(/\) t1 t2 = Operation And t1 t2

(==>) :: Term -> Term -> Term
(==>) t1 t2 = Operation Impl t1 t2

(<==>) :: Term -> Term -> Term
(<==>) t1 t2 = Operation Iff t1 t2

(!<==>) :: Term -> Term -> Term
(!<==>) t1 t2 = Operation Niff t1 t2

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

showTerm (Operation operador (Var i) (Var j)) = showTerm(Var i) ++ show operador ++ showTerm(Var j)
showTerm (Operation operador (Var i) t) = showTerm(Var i) ++ " \\/ (" ++ showTerm(t) ++ ")"
showTerm (Operation operador t (Var i)) = "(" ++ showTerm(t) ++ ")" ++ " \\/ " ++ showTerm(Var i)
showTerm (Operation operador t1 t2) = "(" ++ showTerm t1 ++ ") \\/ (" ++ showTerm t2 ++ ")"

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
