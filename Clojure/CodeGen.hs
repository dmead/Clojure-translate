{- |
The 'CodeGen' module provides the transformer from a clojure AST into executeable code

class instances for Generateable of clojure ASTS are provided

-}

module Clojure.CodeGen where

import Data.List


import Clojure.Syntax



{- | The 'Generateable' class is used for types representing clojure ASTS 
 which we want to traxnsform into executable code

Every dependant type of a clojure AST is generateable

-}


class Generateable a where 
    gen  :: a -> String

instance Generateable Namespace where
   gen a = genNamespace a
    
instance Generateable Sexp where
    gen x = gensexp x 0





{- | 
   'genNamespace' converts the AST of a clojure namespace to executeable code

   for now, namespace constructs are ommited for simplicity

-}

genNamespace :: Namespace -> String
genNamespace (Namespace name functions)  = concat $ map (\x -> (gensexp x 0)) functions



{- | 'gensexp' converts the AST of an S-expression into executable code
   

   Each special form for clojure needs it's own forms, so we'll generate the code accordingly

-}

fromPattern (Pat x) = x


gensexp :: Sexp -> Int -> String
gensexp Nil spaces = (indent spaces) ++ "()"
gensexp WildCard spaces = "_"
gensexp Cons spaces= "cons"
gensexp (Atomic x) spaces = gen x
gensexp (Func name  pairs) spaces = 
    genfunction name pairs spaces
gensexp (List []) spaces = "()"
gensexp (List x) spaces = 
    "(list " ++ (foldr (\y -> (((gen y)++ " ") ++)) [] x) ++ ")"
gensexp (Plist []) spaces = "()"
gensexp (Plist x) spaces = 
    "( " ++ (foldr (\y -> (((gen y)++ " ") ++)) [] x) ++ ")"
gensexp (Apply x y) spaces =  "( " ++ (gen x) ++ (genParam y)++ ")"
gensexp (InfixApply x op z) spaces = 
    "(" ++ (gensexp op spaces) ++ " "
                  ++ (genParam x) ++ " " 
                  ++ (genParam z) ++ ")"
gensexp (PInfixApply x op z) spaces = 
    "(" ++ (gensexp op spaces) ++ " "
                  ++ (gen x) ++ " " 
                  ++ (gen z) ++ ")"
gensexp (Lambda params body) spaces = genLambda (Lambda params body)

gensexp (IF x y z) spaces = "(if" ++ (gen x) ++ (gen y) ++(gen z) ++ ")"
gensexp (ListComp exp quals) spaces = "(listcomp (" ++ (gen exp) ++ 
                               (foldr (\x -> ((genStmt x) ++)) [] quals) ++") () ())"
 


genLambda (Lambda exp body) = "(fn [" ++ (gen exp)++ "] " ++ (gen body) ++ ")" 





--genParam (Atomic (Ident x )) = "(quote " ++  x ++ ")"
genParam (Atomic (Ident x )) = x

genParam (List []) = "()"
genParam (Plist []) = "()"
genParam (Plist x) = 
    "( " ++ (foldr (\y -> (((gen y)++ " ") ++)) [] x) ++ ")"
genParam (List x) = 
    "(list " ++ (foldr (\y -> (((genParam y)++ " ") ++)) [] x) ++ ")"
genParam x = gen x

genpair :: (Sexp, Sexp) -> String
genpair (pattern, function) = "(match params " ++ (gen pattern) ++ " " ++ "\'"++(gen function) ++ " )"

genbindpair :: [(Sexp,Sexp)] -> Int -> String
genbindpair [] num =  " "
genbindpair ((pat,func):xs) num = (replicate  4 ' ') ++ 
                                  "b"++(show num)++ 
                                  "  (match '" ++(gen pat) ++ " params" ++")" ++ "\n"
                                  ++ (genbindpair xs (num+1))
{-
genBody (Apply x y) = gen (Apply x y)
genBody (InfixApply x y z) =  gen (InfixApply x y z)
--genBody (List x) = gen (List x)
genBody x = "(quote  "++ gen x ++ ")"
-}

--genBody (Atomic (Ident x)) = "(quote " ++ x ++ ")"
genBody (Atomic (Ident x)) = x
genBody x = gen x



genStmt (Gen e1 e2) = "(" ++ (gen e1) ++ " <- " ++ (gen e2) ++ ")"
genStmt (Qualifier e) = gen e

--(> (count bindings0) 0)
gencondpair :: [(Sexp,Sexp)] -> Int -> String
gencondpair [] num = " "
gencondpair ((pat,func):xs) num = " (matches b"++(show num)++ " ) (eval (applyBinds b"++(show num) 
                                  ++ " '" ++ (genBody func) ++ ")) \n" ++
                                  (gencondpair xs (num+1))

genfunction :: Sexp -> [(Sexp, Sexp)] -> Int -> String
genfunction name pairs indent = 
    let bindings = (genbindpair pairs 0)
        matches = gencondpair pairs 0 
    in "(defn " ++ (gen name) ++ "[params]" ++ "\n" ++
        "  (let [ \n" ++ bindings ++ "       ]\n" ++
            "    (cond \n" ++  
            matches++ "\n     true (list :patternmatchfail " ++ (gen name) ++ " params) )))\n\n\n"

{- | 'genfunction' does the code generation for a function definition form. 

   IE

   >(defn name args
   >    (body))


   or, if the source haskell had multiple patterns


   > (defn name args
   >    (cond  (match args pat body)
   >           (match args pat body) 
   >           (match args pat body) )

-}

instance Generateable Atom where
    gen x = genatom x


--instance Generateaboe Pattern where
 --   gen x
{- | 'genatom' generates the the code for  non list items in an s-expression


as in the list

> ((1 2 3) 1 2.0 4 fifty nil)

the atomic parts are 1,2,3 1, 2.0, 4 and fifty





-}


genatom :: Atom -> String
genatom (Lit x) = (show x)
genatom (Int x) = (show x)
genatom (String x) = x
genatom (Ident x) = x
genatom (Var x) = x
genatom (Symbol x) = x


{- | 
   'indent' indents a line some spaces 

-}

indent x = replicate x ' '
