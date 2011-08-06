(defn trav[params]
  (let [ 
    b0  (match `() params)
    b1  (match `(cons   x  xs) params)
 lparam (gensym "l") 
lbinds (gensym "b1")  ]
    (cond 
 (matches b0 ) (eval (applyBinds b0`())) 
 (matches b1 ) (eval (applyBinds b1`(cons  x (trav xs)))) 
 
 true (list :patternmatchfail trav params) )))


