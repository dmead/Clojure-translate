(defn tzip[params]
  (let [ 
    b0  (match `(() () ) params)
    b1  (match `((cons   x  xs) (cons   y  ys) ) params)
      lparam (gensym "l") 
     lbinds (gensym "b1")  ]
    (cond 
 (matches b0 ) (eval (applyBinds b0 `())) 
 (matches b1 ) (eval (applyBinds b1 `(cons  (list x y ) (tzip (list xs ys ))))) 
 
 true (list :patternmatchfail tzip params) )))


(defn listid[params]
  (let [ 
    b0  (match `n params)
      lparam (gensym "l") 
     lbinds (gensym "b1")  ]
    (cond 
 (matches b0 ) (eval (applyBinds b0 `(flatmap (list (fn [~lparam](cons  ~lparam ())) n )))) 
 
 true (list :patternmatchfail listid params) )))


(defn squareall[params]
  (let [ 
	b0  (match `l params)
	lparam (gensym "l")
	]
    (cond 
     (matches b0 ) (eval (applyBinds b0 `(flatmap (list (fn [~lparam](cons  (* ~lparam ~lparam) ())) l )))) 
 
     true (list :patternmatchfail squareall params) )))


(defn squareall[params]
  (let [
	b0  (match `l params)
	lparam (gensym "l")
	        lbinds (gensym "b1")  ](defn qs[params]
  (let [ 
	b0  (match `() params)
	b1  (match `(cons   h  t) params)
	lparam (gensym "l") 
	lbinds (gensym "b1")  ]
    (cond 
     (matches b0 ) (eval (applyBinds b0 `())) 
     (matches b1 ) (eval (applyBinds b1 `(++ (qs (flatmap (list (fn [~lparam](if (<= ~lparam h)(cons  ~lparam ())())) t ))) (cons  h (qs (flatmap (list (fn [~lparam](if (> ~lparam h)(cons  ~lparam ())())) t ))))))) 
 
     true (list :patternmatchfail qs params) )))


(defn sort[params]
  (let [ 
    b0  (match `() params)
    b1  (match `(cons   x  xs) params)
      lparam (gensym "l") 
     lbinds (gensym "b1")  ]
    (cond 
     (matches b0 ) (eval (applyBinds b0 `())) 
     (matches b1 ) (eval (applyBinds b1 `(++ (sort (flatmap (list (fn [y](if (< y x)(cons  y ())())) xs ))) (++ (list x ) (sort (flatmap (list (fn [y](if (>= y x)(cons  y ())())) xs ))))))) 
 
 true (list :patternmatchfail sort params) )))


(defn vecAdd[params]
  (let [ 
	b0  (match `(xs ys ) params)
	lparam (gensym "l")
	lparam1 (gensym "l") 
	lbinds (gensym "b1")  ]
    (cond 
     (matches b0 )  (applyBinds b0 `(flatmap (list (fn [[~lparam ~lparam1] ](cons  (+ ~lparam ~lparam1) ())) (tzip (list xs ys )) )))
     
     true (list :patternmatchfail vecAdd params) )))


(defn singletons[params]
  (let [ 
    b0  (match `xs params)
      lparam (gensym "l") 
     lbinds (gensym "b1")  ]
    (cond 
 (matches b0 ) (eval (applyBinds b0 `(flatmap (list (fn [(x )](cons  x ())) xs )))) 
 
 true (list :patternmatchfail singletons params) )))


(defn c1[params]
  (let [ 
    b0  (match `_ params)
      lparam (gensym "l") 
     lbinds (gensym "b1")  ]
    (cond 
 (matches b0 ) (eval (applyBinds b0 `(flatmap (list (fn [xs](flatmap (list (fn [(3 x )](cons  x ())) xs ))) (list (list (list 1 2 ) (list 3 4 ) ) (list (list 5 4 ) (list 3 2 ) ) ) )))) 
 
 true (list :patternmatchfail c1 params) )))


(defn cartesian[params]
  (let [ 
    b0  (match `(s1 s2 ) params)
      lparam (gensym "l") 
     lbinds (gensym "b1")  ]
    (cond 
 (matches b0 ) (eval (applyBinds b0 `(flatmap (list (fn [x](flatmap (list (fn [y](cons  (list x y ) ())) s2 ))) s1 )))) 
 
 true (list :patternmatchfail cartesian params) )))


