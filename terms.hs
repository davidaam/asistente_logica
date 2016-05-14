{-# LANGUAGE FlexibleInstances #-}
module Asistente_pruebas where

import Term
import Equation	
import Theorems


-- Asumimos que la entrada es correcta
data Sust = Sust Term Term

(=:) :: Term -> Term -> Sust
(=:) t1 s1 = Sust t1 s1

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
	sustVar (Var v1) (Sust sustitucion variable) = (abstraer variable (Var v1)) sustitucion

instance Instantiable (Term, Sust, Term) where
	sustVar (Var v1) (t1, Sust t2 s1, s2)
		| (Var v1) == s1 = (abstraer (Var v1) (Var v1)) t1
		| (Var v1) == s2 = (abstraer (Var v1) (Var v1)) t2
		| otherwise = Var v1

instance Instantiable (Term, Term, Sust, Term, Term) where
	sustVar (Var v1) (t1, t2, Sust t3 s1, s2, s3)
		| (Var v1) == s1 = (abstraer (Var v1) (Var v1)) t1
		| (Var v1) == s2 = (abstraer (Var v1) (Var v1)) t2
		| (Var v1) == s3 = (abstraer (Var v1) (Var v1)) t3
		| otherwise = Var v1

leibniz :: Equation -> Term -> Term -> Equation
leibniz (Equation e1 e2) tz (Var z) = Equation (sustituir tz (Sust e1 (Var z))) (sustituir tz (Sust e2 (Var z))) 

infer :: (Instantiable s) => Float -> s -> Term -> Term -> Equation
infer n sus (Var z) tz = leibniz (instantiate (prop n) sus) tz (Var z)

step :: (Instantiable s) => Float -> s -> Term -> Term -> Term -> Term
step n sus (Var z) tz tini
	| tini == e1 = e2
	| tini == e2 = e1
	| otherwise = error "Invalid inference rule"
	where Equation e1 e2 = infer n sus (Var z) tz

proof :: Equation -> IO Term
proof (Equation e1 e2) = print e1 >>= \_ -> return e1

done :: Term -> Equation -> IO Bool
done tfin (Equation e1 e2)
	| e2 == tfin = putStrLn "proof successful" >>= \_ -> return True
	| otherwise = putStrLn "uncomplete proof" >>= \_ -> return False

-- Funciones dummy --
with = id
using = id
lambda = id

statement :: (Instantiable s) => Term -> Float -> s -> Term -> Term -> IO Term
statement tini n sus (Var z) tz = return (step n sus (Var z) tz tini)
