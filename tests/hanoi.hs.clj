(defn dohanoi[params]
  (let [ 
    b0  (match `(0 _ _ _ ) params)
    b1  (match `(n from to using ) params)
      lparam (gensym "l") 
     lbinds (gensym "b1")  ]
    (cond 
 (matches b0 ) (eval (applyBinds b0`())) 
 (matches b1 ) (eval (applyBinds b1`(++ (dohanoi (list (- n 1) from using to )) (++ (list (list from to ) ) (dohanoi (list (- n 1) using to from )))))) 
 
 true (list :patternmatchfail dohanoi params) )))


(defn hanoi[params]
  (let [ 
    b0  (match `n params)
      lparam (gensym "l") 
     lbinds (gensym "b1")  ]
    (cond 
 (matches b0 ) (eval (applyBinds b0`(dohanoi (list n 1 3 2 )))) 
 
 true (list :patternmatchfail hanoi params) )))


