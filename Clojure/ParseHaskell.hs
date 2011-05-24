module Translation where

--import Language.Haskell.Exts
--import Language.Haskell.Exts.Build
--import Language.Haskell.Exts.Comments
--import Language.Haskell.Exts.Extension
--import Language.Haskell.Exts.Fixity
--import Language.Haskell.Exts.Parser
--import Language.Haskell.Exts.Pretty
--i--mport Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax as HS


import System.IO.Unsafe
import Sexp


printastSrc x = 
    do y <- return $ fromParseResult $ unsafePerformIO  $ parseFile x
       print y
       putStr $ prettyPrint y
       return ()


srcFromfile x = 
    do y <- return $ fromParseResult $ unsafePerformIO $ parseFile x
       return y





translateModule :: Module -> Namespace
translateModule (Module _  modname _  _ _ _ decl) = 
    Namespace "thisNamespace" (foldr (\x -> ((translateDecl x)++)) [] decl)


translateDecl :: Decl -> [Sexp]
translateDecl (FunBind matches) = map (translateMatch) matches


translateQName :: QName -> Atom
translateQName (UnQual x) = translateName x

translateQOP :: QOp -> Sexp
translateQOP (QVarOp x) = Atomic (translateQName x)



translateExp :: Exp -> Sexp
translateExp (HS.Var x ) = 
    Atomic (translateQName x)
translateExp (HS.InfixApp a sym b) = 
    InfixApply (translateExp a) (translateQOP sym) (translateExp b)




translateMatch :: Match -> Sexp
translateMatch (Match _  name patlist  _   (UnGuardedRhs rhs) binds) 
    = 
      let sexpPattern = map (translatePattern) patlist
      in Func (translateName name) (Pat sexpPattern)  [(Pat sexpPattern, translateExp rhs)]
                  


translateLiteral::  Literal -> Atom
translateLiteral (HS.Char x) = (Sexp.Lit x)

translateName :: Name -> Atom
translateName (HS.Ident x) = Sexp.Ident x
translateName (HS.Symbol x) = Sexp.Symbol x

translatePattern :: Pat -> Atom
translatePattern (PVar x) = (translateName x)
translatePattern (PLit (Char x)) = Sexp.Lit x


translateFile x = 
    let haskellast = unsafePerformIO $ srcFromfile x
    in (show (translateModule haskellast))