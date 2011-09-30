{- |
The 'Translate' module implements a transformational grammer 
that converts an AST for haskell into one for clojure

This module uses the haskell-src-ext AST to represent haskell.
It transforms those types into the clojure representation 
found in Clojure.Sytnax

-}


module Clojure.Translate where

import Language.Haskell.Exts.Syntax as HS
import Clojure.Syntax as Sexp
import Clojure.DeSugar as DS
import Clojure.AstTransform

class Translateable a where
    translate :: a -> Namespace


instance Translateable Module where
    translate a = translateModule a


--bind (Gen e1 e2) rhs  = Let (Binds [(e1, e2)]) (bind rhs)
--bind (Qualifier exp) rhs = Do exp exp
--bind (LetStmt (Binds [(e1,e2)])) = 



{- | 'translateModule' translates a haskell module into a clojure namespace.
   a module usually contains information about imports/exports and compiler directives
as well as a list of declarations. For now, we're just preseving the name along with declarations.
-}

modstring (ModuleName x) = x 
translateModule :: Module -> Namespace
translateModule (Module _  modname _  _ _ _ decl) = 
    Namespace (modstring modname) ( map (translateDecl) decl)


{- | 'translateDecl' translates a declaration found under a haskell module to a clojure
S-expression. this could be almost anything listed in the haskell spec, but is just function 
bindings for now.
-}



matchname (Match _  name _  _   _ _ ) =  name

translateDecl :: Decl -> Sexp
translateDecl (FunBind matches) = 
    let funcname = translateName . matchname . head $ matches        
    in Func funcname (map (translateMatch False ) matches)
--    in unpackLambdas $ Func funcname (map (translateMatch False ) matches)

{-
translateDecl (PatBind srcloc pat (Maybe atype) rhs binds) =
    let funcname = translateName . matchname . head $ matches
    in 
-}

{- | 
   'translateQName' translates qualified names into a list atom 
-}

translateQName :: QName -> Sexp
translateQName (UnQual x) = translateName x
translateQName (Special x) =  translateSpecial x
 

translateSpecial :: SpecialCon -> Sexp
translateSpecial HS.Cons  = Sexp.Cons


{- | 
   'translateQOP' translates qualified and unqualified operators into S-expressions
-}



--translate bindings declarations, as opposed to top level ones which have different syntax
translateBDecl  (PatBind _ pat _ (UnGuardedRhs exp) binds)  = (translatePattern pat, translateExp exp)


translateBinds (BDecls decls) = Sexp.Binds $ map translateBDecl decls


--translateStmt :: HS.Stmt -> Sexp.Stmt
translateStmt (HS.Generator _ pat exp) = Sexp.Gen (translatePattern pat) (translateExp exp)
translateStmt (HS.Qualifier exp) = Sexp.Qualifier (translateExp exp)
translateStmt (HS.LetStmt binds) = Sexp.LetStmt (translateBinds binds)

translateQualStmt (QualStmt stmt) = translateStmt stmt

translateQOP :: QOp -> Sexp
translateQOP (QVarOp x) = translateQName x
translateQOP (QConOp x) = translateQName x


{- |
   'translateExp' translates haskell expressions into equivilent S-expressions
-}

translateExp :: Exp -> Sexp
translateExp (Con x ) = translateQName x
translateExp (HS.Var x ) = translateQName x
translateExp (HS.Lit x) = translateLiteral x
translateExp (HS.InfixApp a sym b) = InfixApply (translateExp a) (translateQOP sym) (translateExp b)
translateExp (App x y) = Apply (translateExp x) (translateExp y)
--translateExp (HS.List []) = Sexp.Nilc
translateExp (HS.List x) = Sexp.List (map translateExp x)
translateExp (HS.Tuple x) = Sexp.List (map translateExp x)
translateExp (Paren x) = translateExp x
translateExp (HS.If x y z ) = Sexp.IF (translateExp x) (translateExp  y) (translateExp z)
translateExp (HS.Lambda loc pats exp) = 
    if (length pats == 1) then
    Sexp.Lambda (map (translatePattern) pats) (translateExp exp)
    else 
    Sexp.Lambda [translatePattern (head pats)] (translateExp (HS.Lambda loc (tail pats) exp))

translateExp (HS.ListComp exp stmts) = translateExp $ DS.deSugar  $ (HS.ListComp exp stmts)




{- | 'translateMatch' translates a match section of a function declaration
  into a clojure function definition
 -}

--translateMatch :: Match -> Sexp
--translateMatch curried (Match _  name (p1:[])  _    (UnGuardedRhs rhs) binds)  =
 --           (BMatch (translatePattern p1, translateExp rhs))

translateMatch curried (Match _  name (p1:[])  _    (UnGuardedRhs rhs) binds)  =
    if (curried == True) 
    then Sexp.Lambda [translatePattern p1]
             (BMatch (translatePattern p1, translateExp rhs))
    else (BMatch (translatePattern p1, translateExp rhs))

translateMatch curried (Match _  name (p1:pats)  _   (UnGuardedRhs rhs) binds) 
  =   if (curried == True)
      then Sexp.Lambda [translatePattern p1] 
               (BMatch (translatePattern p1, 
            (translateMatch True (Match (SrcLoc "" 0 0) name  (pats) Nothing (UnGuardedRhs rhs) binds))))
      else           
          (BMatch 
           (translatePattern p1,     
                (translateMatch True (Match (SrcLoc "" 0 0) name  (pats) Nothing (UnGuardedRhs rhs) binds))))
           


                  

{- | 'translateLiteral' converts a haskell literal into an s-expression atom

-}
--translateLiteral::  Literal -> Atom
translateLiteral (HS.Char x) = Atomic (Sexp.Lit x)
translateLiteral (HS.Int x) = Atomic (Sexp.Int x)

--translateName :: Name -> Atom
translateName (HS.Ident x) = Atomic $ Sexp.Ident x
translateName (HS.Symbol x) = Atomic $ Sexp.Symbol x




translatePattern (PVar x) = translateName x
translatePattern (PInfixApp e1 op e2) = PInfixApply (translatePattern e1) (translateQName op) (translatePattern e2)
translatePattern (PParen x) = translatePattern x
--translatePattern (PList []) = Nil
translatePattern (PList x) = Sexp.Plist (map (translatePattern) x)
translatePattern (HS.PTuple x) = Sexp.Plist (map translatePattern x)
translatePattern (HS.PWildCard) = WildCard
translatePattern (PLit x) = translateLiteral x