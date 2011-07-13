(defn add[params]
  (let [ 
	b0  (match `x params)
	lparam (gensym "l") 
	lbinds (gensym "b1")  ]
    (cond 
     (matches b0) (eval (applyBinds b0`(fn [~lparam] (let [~lbinds (match `y~lparam)] 
						       (cond (matches ~lbinds) (eval (applyBinds ~lbinds `(+ x y)))))))) 
     
     true (list :patternmatchfail add params) )))


(defn add1[params]
  (let [ 
	b0  (match `(x y ) params)
	lparam (gensym "l") 
	lbinds (gensym "b1")  ]
    (cond 
     (matches b0 ) (eval (applyBinds b0 `(+ x y))) 
     
     true (list :patternmatchfail add1 params) )))

