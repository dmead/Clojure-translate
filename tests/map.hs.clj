(defn add1[params]
  (let [ 
    b0  (match 'x params)
        ]
    (cond 
 (matches b0 ) (eval (applyBinds b0 '(+ x 1))) 
 
     true (list :patternmatchfail add1 params) )))


(defn mymap[params]
  (let [ 
    b0  (match '( f () ) params)
    b1  (match '( f (cons x xs) ) params)
        ]
    (cond 
 (matches b0 ) (eval (applyBinds b0 '())) 
 (matches b1 ) (eval (applyBinds b1 '(cons ( fx) ( mymap(list f xs ))))) 
 
     true (list :patternmatchfail mymap params) )))


