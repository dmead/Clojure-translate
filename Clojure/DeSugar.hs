module Clojure.DeSugar where

import Language.Haskell.Exts.Syntax as HS



--deSugar (Listcomp exp (q:qs)) =

--;;flatmap wants a curried version. need some fucking curry that really works (might just use clojure map)
--rule (a) page 132

deSugar (HS.ListComp exp ((QualStmt (Generator _ gpat gexp)):qs)) 
    = (App 
       (Var (UnQual (Ident "flatmap")))
       (Tuple [(Lambda (SrcLoc "" 0 0) [gpat]
                           (deSugar (ListComp exp qs))), gexp])
      )

--rule (b)
deSugar (ListComp  exp ((QualStmt (Qualifier guard)): qs)) = 
    If (deSugar guard)
           (deSugar (ListComp exp qs))
           (List [])
          

--rule (c) 
deSugar (ListComp exp []) = (App (App (Var (Special Cons)) exp) (List []))




                                                          
--deSugar 


                                                                        

deSugar x = x
                                                                        
                                                                                                                                        
                                                                
                                                                
                                                                
                                                                    




    

