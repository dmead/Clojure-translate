test 0 ="zero"
test x | x > 2 = "bigger than two"
       | True = "less than two"

{-

Module (SrcLoc {srcFilename = "guardtest.hs", srcLine = 1, srcColumn = 1}) (ModuleName "Main") [] Nothing (Just [EVar (UnQual (Ident "main"))]) [] [FunBind [

Match (SrcLoc {srcFilename = "guardtest.hs", srcLine = 1, srcColumn = 1}) (Ident "test") [PLit (Int 0)] Nothing (UnGuardedRhs (Lit (String "zero"))) (BDecls []),

Match (SrcLoc {srcFilename = "guardtest.hs", srcLine = 2, srcColumn = 1}) (Ident "test") [PVar (Ident "x")] 

Nothing (GuardedRhss 
                  [GuardedRhs (SrcLoc {srcFilename = "guardtest.hs", srcLine = 2, srcColumn = 8}) 
        [Qualifier (InfixApp (Var (UnQual (Ident "x"))) (QVarOp (UnQual (Symbol ">"))) (Lit (Int 2)))] 
        (Lit (String "bigger than two")),
        GuardedRhs (SrcLoc {srcFilename = "guardtest.hs", srcLine = 3, srcColumn = 8}) 
                   [Qualifier (Con (UnQual (Ident "True")))] (Lit (String "less than two"))]) (BDecls [])]

]
module Main (main) where
test 0 = "zero"
test x
  | x > 2 = "bigger than two"
  | True = "less than two"