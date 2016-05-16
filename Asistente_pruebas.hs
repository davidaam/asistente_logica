module Asistente_pruebas (
    module Asistente_pruebas,
    module Sust,
    module Theorems
) where

import Sust
import Theorems

-- Aplica leibniz sobre la equacion e1 === e2, con E(z) = tz
leibniz :: Equation -> Term -> Term -> Equation
leibniz (Equation e1 e2) tz (Var z) = Equation (sustituir tz (Sust e1 (Var z))) (sustituir tz (Sust e2 (Var z))) 

-- Dado el número de un teorema, aplica la sustitución sus sobre ese teorema y luego
-- leibniz en la ecuación resultante
infer :: (Instantiable s) => Float -> s -> Term -> Term -> Equation
infer n sus (Var z) tz = leibniz (instantiate (prop n) sus) tz (Var z)

-- Aplica un teorema partiendo de un termino inicial, y devuelve el termino resultante
-- de aplicar la inferencia
step :: (Instantiable s) => Float -> s -> Term -> Term -> Term -> Term
step n sus (Var z) tz tini
    | tini == e1 = e2
    | tini == e2 = e1
    | otherwise = error "Invalid inference rule"
    where Equation e1 e2 = infer n sus (Var z) tz

proof :: Equation -> IO Term
proof (Equation e1 e2) = print e1 >>= \_ -> return e1

done :: Equation -> Term -> IO ()
done (Equation e1 e2) tfin 
    | e2 == tfin = putStrLn "proof successful"
    | otherwise = putStrLn "proof unsuccessful"

-- Dummy variables --
with = 0
using = 0
lambda = 0

-- Los parametros que tienen como tipo variable a, son aquellos que son ignorados
statement :: (Instantiable s) => Float -> a -> s -> a -> a -> Term -> Term -> Term -> IO Term
statement n _ sus _ _ (Var z) tz tini = return (step n sus (Var z) tz tini)
    >>= \t -> putStrLn ("=== <Statement " ++ show n ++ " with " ++ show sus ++ " using lambda " ++ show (Var z) ++ " (" ++ show tz ++ ")>")
    >>= \_ -> print t
    >>= \_ -> return t
