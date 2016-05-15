module Theorems where 
import Equation

prop :: Float -> Equation 
prop num
  | num == 3.1 = (p <==> q) <==> r === p <==> (q <==> r)
  | num == 3.2  = (p <==> q) === (q <==> p)
  | num == 3.21 = (p <==> q) <==> (q <==> p) === true
  | num == 3.3  = (p <==> p) === true
  | num == 3.31 = (p <==> q) <==> q === p
  | num == 3.4  = p === (p <==> true)
  | num == 3.5 = (p <==> q) <==> q === p 
  | otherwise = error "The statement doesn't exists"