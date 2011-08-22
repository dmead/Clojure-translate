{- |
The 'Syntax' module provides an AST in types of (a subset of) clojure

These types express the structure of s-expressions while having special forms for
things like function defintition, patterns, pattern matching and different types of list atoms.

-}

module Clojure.Syntax where



import Data.Data
import Data.Typeable
import Data.Generics.PlateData


{- | a container for clojure namespaces.    
for now, this is just a name with a list of S-expressions
-}
data Namespace = Namespace String [Sexp]      
     deriving (Show, Eq) 


fromNamespace (Namespace name funcs) = funcs


{- | 
Pattern is a container for pattern lists
-}
data Pattern = Pat [Sexp]      
             | Ptrue
             -- ^ a catch-call form for patterns
               deriving (Show, Eq)

{- |
   Atom is a type for list contents that have a primative type
-}

data Stmt = Gen Sexp Sexp
          | Qualifier Sexp
          | LetStmt Binds 
         deriving (Eq, Show, Data, Typeable)
            

data Atom = Lit Char 
     -- ^ a literal character inside an S-exp
     | String String 
     -- ^ A Primative String     
     | Ident String 
     -- ^ Idenitifer For Named Functions And Data structures
     | Var String 
          -- ^ identifier for variables
     | Symbol String
     | Int Integer
       -- ^ a single character used for operators
     deriving (Eq, Show, Data, Typeable)


{- |
   Sexp is the primary representation for S-expressions
   Normally, an S-expression is a s
-}
data Sexp = Atomic Atom 
         | WildCard
         | Void 
         | Cons
         | Lambda Sexp Sexp
         -- ^ cons with it's element as a general sexp
         | IF Sexp Sexp Sexp             
         -- ^ if expr then expr else expr (conditionals)
         | Apply Sexp Sexp               
         -- ^ a general form for function application
         | InfixApply Sexp Sexp Sexp     
         | PInfixApply Sexp Sexp Sexp     
         -- ^ a general form for infix function calls
         | BMatch (Sexp, Sexp)
         | Func Sexp [Sexp]
         -- ^function def and it's pattern cases
         | List [Sexp]
         -- ^ regular s-expression list
         | Tuple [Sexp]
         | Ptuple [Sexp]
         | Plist [Sexp]
         | ListComp Sexp [Stmt]
         | Nil      
         | Let Binds Sexp
         | Do [Sexp]
           deriving (Show, Eq, Data, Typeable)


--instance Uniplate Sexp
--instance Uniplate Atom
--instance Uniplate Namespace


data Binds = Binds [(Sexp, Sexp)]
           deriving (Show, Eq, Data, Typeable)


listIdents x = [y | (Atomic (Ident y)) <- universe x]




replaceIdent a b  = transform $ (\q -> case q of 
                                     (Atomic (Ident i)) -> 
                                         if (i == b) then  (Atomic (Ident a))
                                         else  (Atomic (Ident i))
                                     z -> z)



