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



import Language.Haskell.Exts
import Clojure.Syntax
import Clojure.Translate
import Clojure.CodeGen
import System.IO.Unsafe
import System.IO
import System.Environment


{- |
   'astfromFile' calls the parser from 

-}

gethaskellast x =  fromParseResult $ unsafePerformIO  $ parseFile x
getclojureast x =  translate $ fromParseResult $ unsafePerformIO  $ parseFile x


printastSrc x = 
    do y <- return $ fromParseResult $ unsafePerformIO  $ parseFile x

       putStr $ prettyPrint y ++ "\n"
       return ()
              
translateFile x = 
    do haskellAST <- return $ gethaskellast x
       print haskellAST
       clojureAST <- return $ translate haskellAST
       print clojureAST
       outfile <- openFile (x++".clj") WriteMode
       hPutStr outfile (gen clojureAST)
       hClose outfile
       return ()


test =  "/home/dmead/Translator/tests/lists.hs"
test1 = "/home/dmead/Translator/tests/add.hs"
test2 =  "/home/dmead/Translator/tests/hello.hs"
test3 =  "/home/dmead/Translator/tests/map.hs"
test4 =  "/home/dmead/Translator/tests/tupleadd.hs"
test5 =  "/home/dmead/Translator/NewTests-RWW/myflip.hs"
test6 =  "/home/dmead/Translator/NewTests-RWW/myrest.hs"
test7 =  "/home/dmead/Translator/tests/hanoi.hs"
test8 =  "/home/dmead/Translator/tests/quicksort.hs"
test9 =  "/home/dmead/Translator/tests/lambda.hs"
test10 = "/home/dmead/Translator/tests/comprehensions.hs"
test11 = "/home/dmead/Translator/tests/compSimple.hs"
test12 = "/home/dmead/Translator/tests/let.hs"
test13 = "/home/dmead/Translator/tests/dosimple.hs"


main = do args <- System.Environment.getArgs
          translateFile (head args)
          return ()
               
   


