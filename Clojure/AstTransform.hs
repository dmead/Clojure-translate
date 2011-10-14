module Clojure.AstTransform where

import Clojure.Syntax as Sexp
import Data.Data
import Data.Typeable
import Data.Generics.PlateData




listIdents x = [y | (Atomic (Ident y)) <- universe x]

listall x = [y | y <- universe x]

replaceident a b  = transform $ (\q -> case q of 
                                     (Atomic (Ident i)) -> 
                                         if (i == b) then  (Atomic (Ident a))
                                         else  (Atomic (Ident i))
                                     z -> z)



replacenode b a = transform $ (\q -> if (q == b) then a
                                     else q)

gensympairs = transform $ (\q -> case q of 
                                (Lambda (x:[]) y) -> (replacenode x (Genpair ((Gensym x), x)) (Lambda [x] y))
                                x -> x)


unpackLambdas = transform $ (\q -> case q of 
                                (Lambda (x:[]) y) -> (Lambda [(Atomic (Ident "params"))]
                                                                 (BMatch (x,y)))
                                x -> x)
