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



gparen x = "(" ++ x ++ ")"
gbrackets x = "[" ++ x ++ "]"
backquote x = "`" ++ x
backtick x = "'" ++ x
gnil = "()"
space = "  "
--space = ++ . " ".  ++

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
gensexp Nil spaces = (indent spaces) ++ gnil
gensexp WildCard spaces = "_"
gensexp Cons spaces= "cons "
gensexp (Atomic x) spaces = gen x
gensexp (Func name  bodies) spaces =   genfunction name bodies spaces
gensexp (List []) spaces = gnil
gensexp (List x) spaces = 
    gparen $ "list " ++ (foldr (\y -> (((gen y)++ " ") ++)) [] x)
gensexp (Plist []) spaces = gnil
gensexp (Plist x) spaces = 
    gparen $ (foldr (\y -> (((gen y)++ " ") ++)) [] x)
gensexp (Apply x y) spaces =  gparen $ (gen x) ++ " " ++ (genParam y)
gensexp (InfixApply x op z) spaces = 
    gparen $ (gensexp op spaces) ++ " " ++ 
             (genParam x) ++ " " ++ (genParam z) 


gensexp (PInfixApply x op z) spaces = 
    gparen $ (gensexp op spaces) ++ space ++
             (gen x)  ++ space ++ 
             (gen z)
gensexp (Lambda params body) spaces = 
    genLambda (Lambda params body)

gensexp (IF x y z) spaces = gparen $ "if " ++ (gen x) ++ (gen y) ++(gen z)


--this is so broken
--gensexp (ListComp exp quals) spaces = gparen $ "listcomp " ++ (gen exp) ++ 
--                               (foldr (\x -> ((genStmt x) ++)) [] quals) ++ gnil 

gensexp (BMatch (pat, body)) spaces = gencondpair [(BMatch (pat,body))] 0

gensexp x _ = error ("can't gen this:  " ++ (show x))
 
              
--all 
genLambda (Lambda exp body) = 
    gparen $ "fn " ++ (gbrackets $ "~lparam")  ++ (gen body)
--    gparen $ "fn " ++ (gbrackets $ (gen exp))  ++ (gen body)

genatoms :: [String] -> [Char]
genatoms (x:xs) = gparen $  (foldr (\q -> ((q ++ " ") ++)) [] (x:xs))
genvector x = gbrackets $ genatoms x
genLet vec body  =  gparen $ "let "  ++ (genvector vec) ++ body


genParam (Atomic (Ident x )) = x
genParam (List []) = gnil
genParam (Plist []) = gnil
genParam (Plist x) = 
    gparen $  (foldr (\y -> (((gen y)++ " ") ++)) [] x)
genParam (List x) = 
    gparen $ "list " ++ (foldr (\y -> (((genParam y)++ " ") ++)) [] x)
genParam x = gen x


genpair :: (Sexp, Sexp) -> String
genpair (pattern, function) = 
    "(match params " ++ (gen pattern) ++ " " ++ "\'"++(gen function) ++ " )"

genbindpair :: [Sexp] -> Int -> String
genbindpair [] num =  " "
genbindpair ((BMatch (pat,func)):xs) num = (replicate  4 ' ') ++ 
                                  "b"++(show num)++ 
                                  "  (match `" ++(gen pat) ++ " params" ++")" ++ "\n"
                                  ++ (genbindpair xs (num+1))


genBody (Atomic (Ident x)) = x
genBody (Lambda pat (BMatch (p1,body))) = "'(fn [~lparam] (let [~lbinds (match `" ++ (gen pat) ++ "~lparam)] \n " ++
                              "     (cond (matches ~lbinds) (eval (applyBinds ~lbinds " ++ (genBody body) ++  ")))))"
genBody x = "`" ++ gen x


genBodytoplevel (Lambda pat (BMatch (p1, body))) = 
    "`(fn [~lparam] (let [~lbinds (match `" ++ (gen pat) ++ "~lparam)] \n " ++         
 "     (cond (matches ~lbinds) (eval (applyBinds ~lbinds " ++ (genBody body) ++  ")))))"
genBodytoplevel x = (genBody x)

genStmt (Gen e1 e2) = "(" ++ (gen e1) ++ " <- " ++ (gen e2) ++ ")"
genStmt (Qualifier e) = gen e

--(> (count bindings0) 0)
gencondpair :: [Sexp] -> Int -> String
gencondpair [] num = " "
gencondpair ((BMatch  (_ ,Lambda pat body)):morepats) num =
           "  (matches b"++(show num)++ ") (eval (applyBinds b"++(show num)++ " "++
                            (genBodytoplevel (Lambda pat body)) ++ ")) \n" ++
                                                         (gencondpair morepats (num+1))
gencondpair ((BMatch (pat,func)):xs) num = " (matches b"++(show num)++ " ) (eval (applyBinds b"++(show num) 
                                  ++ (genBodytoplevel func) ++ ")) \n" ++
                                  (gencondpair xs (num+1))

                            


genfunction :: Sexp -> [Sexp] -> Int -> String
genfunction name pairs indent = 
    let bindings = (genbindpair pairs 0)
        matches = gencondpair pairs 0 
    in "(defn " ++ (gen name) ++ "[params]" ++ "\n" ++
        "  (let [ \n" ++ bindings ++
                          "lparam (gensym \"l\") \n" ++
                         "lbinds (gensym \"b1\")  ]\n" ++
                          "    (cond \n" ++ matches ++ "\n true (list :patternmatchfail " 
                               ++ (gen name) ++ " params) )))\n\n\n"

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
