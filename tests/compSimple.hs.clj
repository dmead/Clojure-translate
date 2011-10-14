(defn t1[params]
  (let [ 
    b0  (match `q params)
      lparam (gensym "l") 
     lbinds (gensym "b1")  ]
    (cond 
 (matches b0 ) (eval (applyBinds b0 `(flatmap (list (fn [x](cons  x ())) (list 1 2 3 4 5 6 7 8 9 10 ) )))) 
 
 true (list :patternmatchfail t1 params) )))


