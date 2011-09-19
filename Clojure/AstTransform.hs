module Clojure.AstTransform where



import Clojure.Syntax as Sexp
import Data.Data
import Data.Typeable
import Data.Generics.PlateData






listIdents x = [y | (Atomic (Ident y)) <- universe x]



replaceident a b  = transform $ (\q -> case q of 
                                     (Atomic (Ident i)) -> 
                                         if (i == b) then  (Atomic (Ident a))
                                         else  (Atomic (Ident i))
                                     z -> z)







unpackLambdas = transform $ (\q -> case q of 
                                (Lambda (x:[]) y) -> (Lambda [(Atomic (Ident "params"))]
                                                                 (BMatch (x,y)))
                                x -> x) 
                                             
