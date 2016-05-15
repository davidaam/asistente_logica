{-# LANGUAGE FlexibleInstances #-}
module Sust where

import Term
import Equation
-- Asumimos que la entrada es correcta
data Sust = Sust Term Term

(=:) :: Term -> Term -> Sust
(=:) t1 s1 = Sust t1 s1

infixr 1 =:

class (Show s) => Instantiable s where
    sustituir :: Term -> s -> Term
    sustVar :: Term -> s -> Term
    instantiate :: Equation -> s -> Equation
    instantiate (Equation t1 t2) sus = Equation (sustituir t1 sus) (sustituir t2 sus)

    sustituir (Boolean b) _ = Boolean b
    sustituir (Var v1) sus = sustVar (Var v1) sus
    sustituir (Operation operador t1 t2) sus = Operation operador (sustituir t1 sus) (sustituir t2 sus)


instance Instantiable Sust where
    sustVar (Var v1) (Sust sustitucion variable)
        | (Var v1) == variable = sustitucion
        | otherwise = Var v1

instance Instantiable (Term, Sust, Term) where
    sustVar (Var v1) (t1, Sust t2 s1, s2)
        | (Var v1) == s1 = t1
        | (Var v1) == s2 = t2
        | otherwise = Var v1

instance Instantiable (Term, Term, Sust, Term, Term) where
    sustVar (Var v1) (t1, t2, Sust t3 s1, s2, s3)
        | (Var v1) == s1 = t1
        | (Var v1) == s2 = t2
        | (Var v1) == s3 = t3
        | otherwise = Var v1

instance Show Sust where
    show (Sust t (Var z)) = show t ++ "=:" ++ z