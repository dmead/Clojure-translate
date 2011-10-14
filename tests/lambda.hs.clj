(defn add[params]
  (let [ 
    b0  (match `y params)
      lparam (gensym "l") 
     lbinds (gensym "b1")  ]
    (cond 
  (matches b0) (eval (applyBinds b0 `(fn [x](+ x y)))) 
 
 true (list :patternmatchfail add params) )))


