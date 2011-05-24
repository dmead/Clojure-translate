{-

Module (SrcLoc {srcFilename = "simpleadd.hs", srcLine = 1, srcColumn = 1}) 
 (ModuleName "Main") [] 
             Nothing (Just [EVar (UnQual (Ident "main"))]) [] 

[
 FunBind 
 [Match (SrcLoc {srcFilename = "simpleadd.hs", srcLine = 1, srcColumn = 1}) 
 (Ident "add") [PVar (Ident "x"),PVar (Ident "y")] 
  Nothing (UnGuardedRhs (InfixApp (Var (UnQual (Ident "x"))) 
                                  (QVarOp (UnQual (Symbol "+"))) 
                                  (Var (UnQual (Ident "y"))))) 
                                  (BDecls [])]]

-}
add x y = x + y