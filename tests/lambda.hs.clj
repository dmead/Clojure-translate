(defn add[params]
  (let [ 
    b0  (match `y params)
 lparam (gensym "l") 
lbinds (gensym "b1")  ]
    (cond 
  (matches b0) (eval (applyBinds b0 `(fn [~lparam] (let [~lbinds (match `params~lparam)] 
      (cond (matches ~lbinds) (eval (applyBinds ~lbinds `(+ x y)))))))) 
 
 true (list :patternmatchfail add params) )))


