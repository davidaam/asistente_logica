module Equation (
	module Equation,
	module Term
) where

import Term

-- Precedencia y asociatividad de la equivalencia
infixl 1 ===

(===) :: Term -> Term -> Equation
(===) t1 t2 = Equation t1 t2

data Equation = Equation Term Term

instance Eq Equation where
    Equation t1 t2 == Equation t3 t4
        | t1 == t3 && t2 == t4 = True
        | t1 == t4 && t2 == t3 = True
        | otherwise = False

instance Show Equation where
    show (Equation t1 t2) = show t1 ++ " === " ++ show t2
