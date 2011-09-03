(defn add[params]
  (let [ 
    b0  (match `(x y ) params)
 lparam (gensym "l") 
lbinds (gensym "b1")  ]
    (cond 
 (matches b0 ) (eval (applyBinds b0`(+ x y))) 
 
 true (list :patternmatchfail add params) )))


(defn add1[params]
  (let [ 
    b0  (match `x params)
 lparam (gensym "l") 
lbinds (gensym "b1")  ]
    (cond 
  (matches b0) (eval (applyBinds b0 `(fn [~lparam] (let [~lbinds (match `params~lparam)] 
      (cond (matches ~lbinds) (eval (applyBinds ~lbinds ` (matches b0 ) (eval (applyBinds b0`(+ x y))) 
 ))))))) 
 
 true (list :patternmatchfail add1 params) )))


(defn mysum[params]
  (let [ 
    b0  (match `() params)
    b1  (match `(cons   x  xs) params)
 lparam (gensym "l") 
lbinds (gensym "b1")  ]
    (cond 
 (matches b0 ) (eval (applyBinds b0`0)) 
 (matches b1 ) (eval (applyBinds b1`(+ x (mysum xs)))) 
 
 true (list :patternmatchfail mysum params) )))


(defn mymap[params]
  (let [ 
    b0  (match `f params)
    b1  (match `f params)
 lparam (gensym "l") 
lbinds (gensym "b1")  ]
    (cond 
  (matches b0) (eval (applyBinds b0 `(fn [~lparam] (let [~lbinds (match `params~lparam)] 
      (cond (matches ~lbinds) (eval (applyBinds ~lbinds ` (matches b0 ) (eval (applyBinds b0`())) 
 ))))))) 
  (matches b1) (eval (applyBinds b1 `(fn [~lparam] (let [~lbinds (match `params~lparam)] 
      (cond (matches ~lbinds) (eval (applyBinds ~lbinds ` (matches b0 ) (eval (applyBinds b0`(cons  (f x) ((mymap f) xs)))) 
 ))))))) 
 
 true (list :patternmatchfail mymap params) )))


(defn mymap1[params]
  (let [ 
    b0  (match `(f () ) params)
    b1  (match `(f (cons   x  xs) ) params)
 lparam (gensym "l") 
lbinds (gensym "b1")  ]
    (cond 
 (matches b0 ) (eval (applyBinds b0`())) 
 (matches b1 ) (eval (applyBinds b1`(cons  (f x) (mymap1 (list f xs ))))) 
 
 true (list :patternmatchfail mymap1 params) )))


(defn cart[params]
  (let [ 
    b0  (match `xs params)
 lparam (gensym "l") 
lbinds (gensym "b1")  ]
    (cond 
  (matches b0) (eval (applyBinds b0 `(fn [~lparam] (let [~lbinds (match `params~lparam)] 
      (cond (matches ~lbinds) (eval (applyBinds ~lbinds ` (matches b0 ) (eval (applyBinds b0`(flatmap (list (fn [~lparam] (matches b0 ) (eval (applyBinds b0`(flatmap (list (fn [~lparam] (matches b0 ) (eval (applyBinds b0`(cons  (list x y ) ()))) 
 ) ys )))) 
 ) xs )))) 
 ))))))) 
 
 true (list :patternmatchfail cart params) )))


