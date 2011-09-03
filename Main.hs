

{- |
Module: 'Translator'

The top level for haskell to clojure translation. 

This will:

 -Provide functionality to inspecting haskell ASTs via the haskell-src-exts package

 -Use the translform rules from Clojure.Translate to translate a Haskell AST to a clojure equivilant
  into the clojure AST in Clojure.Syntax

 -call the code generator in Clojure.CodeGen and write the results to a file
-}
module Main where



import Language.Haskell.Exts as HS
import Clojure.Syntax as CS
import Clojure.Translate
import Clojure.CodeGen
import Clojure.AstTransform
import System.IO.Unsafe
import System.IO
import System.Environment
import Data.Generics.Uniplate

{- |
   'astfromFile' calls the parser from 

-}



--cgetvars x = [y | (.Var y) <- universe x]



gethaskellast x =  fromParseResult $ unsafePerformIO  $ parseFile x
getclojureast x =  translate $ fromParseResult $ unsafePerformIO  $ parseFile x


printastSrc x = 
    do y <- return $ fromParseResult $ unsafePerformIO  $ parseFile x
       putStr $ prettyPrint y ++ "\n"
       return ()
              
translateFile x = 
    do putStr $ "translating file: " ++ x ++ "\n" 
       haskellAST <- return $ gethaskellast x
       --print haskellAST
       clojureAST <- return $ translate haskellAST
       --print clojureAST
       outfile <- openFile (x++".clj") WriteMode
       hPutStr outfile (gen clojureAST)
       hClose outfile
       return ()


testpath = "/home/"


test =  "tests/lists.hs"
test1 = "tests/add.hs"
--test2 =  "tests/hello.hs"
test2 =  "tests/map.hs"
test3 =  "tests/tupleadd.hs"
test4 =  "tests/hanoi.hs"
test5 =  "tests/quicksort.hs"
test6 =  "tests/lambda.hs"
test7 = "tests/comprehensions.hs"
test8 = "tests/compSimple.hs"
--test9 = "tests/let.hs"
--test10 = "tests/dosimple.hs"
test11 = "tests/curry_example.hs"
test12 = "tests/demo.hs"

tests = [test,test1,test2,test3,test4,test5,test6,test7,test8,{-test9,test10,-}test11,test12]

runall =  mapM_ translateFile tests



main = do args <- System.Environment.getArgs
          translateFile (head args)
          return ()
               
   



