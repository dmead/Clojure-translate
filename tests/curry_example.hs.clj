(defn add3[params]
  (let [ 
    b0  (match `x params)
 lparam (gensym "l") 
lbinds (gensym "b1")  ]
    (cond 
  (matches b0) (eval (applyBinds b0 `(fn [~lparam] (let [~lbinds (match `y~lparam)] 
      (cond (matches ~lbinds) (eval (applyBinds ~lbinds '(fn [~lparam] (let [~lbinds (match `z~lparam)] 
      (cond (matches ~lbinds) (eval (applyBinds ~lbinds `(+ (+ x y) z))))))))))))) 
 
 true (list :patternmatchfail add3 params) )))


