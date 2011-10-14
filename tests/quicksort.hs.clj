(defn filtergte[params]
  (let [ 
    b0  (match `(q () ) params)
    b1  (match `(q (cons   x  xs) ) params)
      lparam (gensym "l") 
     lbinds (gensym "b1")  ]
    (cond 
 (matches b0 ) (eval (applyBinds b0 `())) 
 (matches b1 ) (eval (applyBinds b1 `(if (<= q x)(cons  x (filtergte (list q xs )))(filtergte (list q xs ))))) 
 
 true (list :patternmatchfail filtergte params) )))


(defn filterLT[params]
  (let [ 
    b0  (match `(q () ) params)
    b1  (match `(q (cons   x  xs) ) params)
      lparam (gensym "l") 
     lbinds (gensym "b1")  ]
    (cond 
 (matches b0 ) (eval (applyBinds b0 `())) 
 (matches b1 ) (eval (applyBinds b1 `(if (> q x)(++ (list x ) (filterLT (list q xs )))(filterLT (list q xs ))))) 
 
 true (list :patternmatchfail filterLT params) )))


(defn quicksort[params]
  (let [ 
    b0  (match `() params)
    b1  (match `(cons   x  xs) params)
      lparam (gensym "l") 
     lbinds (gensym "b1")  ]
    (cond 
 (matches b0 ) (eval (applyBinds b0 `())) 
 (matches b1 ) (eval (applyBinds b1 `(++ (quicksort (filterLT (list x xs ))) (++ (list x ) (quicksort (filtergte (list x xs ))))))) 
 
 true (list :patternmatchfail quicksort params) )))


(defn mytake[params]
  (let [ 
    b0  (match `(0 _ ) params)
    b1  (match `(n (cons   x  xs) ) params)
      lparam (gensym "l") 
     lbinds (gensym "b1")  ]
    (cond 
 (matches b0 ) (eval (applyBinds b0 `())) 
 (matches b1 ) (eval (applyBinds b1 `(cons  x (mytake (list (- n 1) xs ))))) 
 
 true (list :patternmatchfail mytake params) )))


(defn fib[params]
  (let [ 
    b0  (match `0 params)
    b1  (match `1 params)
    b2  (match `n params)
      lparam (gensym "l") 
     lbinds (gensym "b1")  ]
    (cond 
 (matches b0 ) (eval (applyBinds b0 `0)) 
 (matches b1 ) (eval (applyBinds b1 `1)) 
 (matches b2 ) (eval (applyBinds b2 `(+ (fib (- n 1)) (fib (- n 2))))) 
 
 true (list :patternmatchfail fib params) )))


(defn nth[params]
  (let [ 
    b0  (match `(0 (cons   x  xs) ) params)
    b1  (match `(n (cons   x  xs) ) params)
      lparam (gensym "l") 
     lbinds (gensym "b1")  ]
    (cond 
 (matches b0 ) (eval (applyBinds b0 x)) 
 (matches b1 ) (eval (applyBinds b1 `(nth (list (- n 1) xs )))) 
 
 true (list :patternmatchfail nth params) )))


