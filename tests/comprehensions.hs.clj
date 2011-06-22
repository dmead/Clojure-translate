(defn tzip[params]
  (let [ 
    b0  (match `( () () ) params)
    b1  (match `( (cons x xs) (cons y ys) ) params)
        ]
    (cond 
 (matches b0 ) (eval (applyBinds b0 `())) 
 (matches b1 ) (eval (applyBinds b1 `(cons (list x y ) ( tzip(list xs ys ))))) 
 
     true (list :patternmatchfail tzip params) )))


(defn listid[params]
  (let [ 
    b0  (match `n params)
        ]
    (cond 
 (matches b0 ) (eval (applyBinds b0 `(listcomp (x(x <- n)) () ()))) 
 
     true (list :patternmatchfail listid params) )))


(defn squareall[params]
  (let [ 
    b0  (match `l params)
        ]
    (cond 
 (matches b0 ) (eval (applyBinds b0 `(listcomp ((* x x)(x <- l)) () ()))) 
 
     true (list :patternmatchfail squareall params) )))


(defn qs[params]
  (let [ 
    b0  (match `() params)
    b1  (match `(cons h t) params)
        ]
    (cond 
 (matches b0 ) (eval (applyBinds b0 `())) 
 (matches b1 ) (eval (applyBinds b1 `(++ ( qs(listcomp (x(x <- t)(<= x h)) () ())) (cons h ( qs(listcomp (x(x <- t)(> x h)) () ())))))) 
 
     true (list :patternmatchfail qs params) )))


(defn sort[params]
  (let [ 
    b0  (match `() params)
    b1  (match `(cons x xs) params)
        ]
    (cond 
 (matches b0 ) (eval (applyBinds b0 `())) 
 (matches b1 ) (eval (applyBinds b1 `(++ ( sort(listcomp (y(y <- xs)(< y x)) () ())) (++ (list x ) ( sort(listcomp (y(y <- xs)(>= y x)) () ())))))) 
 
     true (list :patternmatchfail sort params) )))


(defn vecAdd[params]
  (let [ 
    b0  (match `( xs ys ) params)
        ]
    (cond 
 (matches b0 ) (eval (applyBinds b0 `(listcomp ((+ x y)(( x y ) <- ( tzip(list xs ys )))) () ()))) 
 
     true (list :patternmatchfail vecAdd params) )))


(defn singletons[params]
  (let [ 
    b0  (match `xs params)
        ]
    (cond 
 (matches b0 ) (eval (applyBinds b0 `(listcomp (x(( x ) <- xs)) () ()))) 
 
     true (list :patternmatchfail singletons params) )))


(defn c1[params]
  (let [ 
    b0  (match `_ params)
        ]
    (cond 
 (matches b0 ) (eval (applyBinds b0 `(listcomp (x(xs <- (list (list (list 1 2 ) (list 3 4 ) ) (list (list 5 4 ) (list 3 2 ) ) ))(( 3 x ) <- xs)) () ()))) 
 
     true (list :patternmatchfail c1 params) )))


(defn cartesian[params]
  (let [ 
    b0  (match `( s1 s2 ) params)
        ]
    (cond 
 (matches b0 ) (eval (applyBinds b0 `(listcomp ((list x y )(x <- s1)(y <- s2)) () ()))) 
 
     true (list :patternmatchfail cartesian params) )))


