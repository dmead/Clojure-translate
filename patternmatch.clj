(declare applyBinds)

(def matchfail :matchfail)
(declare matches)
(defn third [x]
 (first (rest (rest x))))

(defmacro consp [x]
  `(or (and (seq? ~x) (= (first ~x)    `cons))
       (= (first ~x)    'cons)))

(defn conselem [x]
 (second x))

(defn conslist [x]
  (third x))


(defn singletonp [x]
 (and (list? x) (= (count x) 1) (symbol? (first x))))

(defn listp [x]
 (= (first x) 'list))

(defn nilp [x]
 (= () x))

(defn atomp[x]
 (not (seq? x)))


(declare match)

(defn zip-aux [x y acc]
 (cond (empty? x) acc
       true (recur (rest x) (rest y) (concat acc (list (list (first x) (first y)) )))))

(defn zip [x y]
 (zip-aux x y ()))

(defn makebind [pat value]
 (cond (= pat value) '()
       (or (seq? value) (list? value))   (seq (list pat (cons 'list value)))
       true (seq (list pat value))))

(defn zipmatch [pat  state]
 (cond (and (empty? state) (empty pat)) '()
      true (concat  (match (first pat) (first state)) (match (rest
							      pat) (rest state)))))


(defmacro mzipmatch [pat  state]
 (cond (and (empty? state) (empty pat)) '()
      true `( ~(first pat) ~(first state) (mzipmatch ~(rest pat) ~(rest state)))))

(defmacro mcount[x]
  (count x))

(defn matchlist [pat state]
 (cond  (atomp state) (list (list :matchfail :matchlistfail)) ;;needseqable from contrib
        (= (count state) (count pat)) (zipmatch pat state)
       true (list (list :matchfail :matchlistfail))))

(defn matchcons [pat state]
 (or (seq? state) (seq? state))
         (cond (empty? state) (seq (list :matchfail :emptylistwithcons))
               true (cons (makebind (conselem pat) (first state))
                    (match (conslist pat) (rest state)))
	       true (list (list :matchfail :matchconsfail))))

(defmacro msecond [[x y z]]
  y)


(defmacro mmatchcons [pat state]
 (or (seq? state) (seq? state))
         (cond (empty? state) `(seq (list :matchfail :emptylistwithcons))
               true `(cons (~(`(eval (msecond ~pat))))(~(first state))
                    (match (conslist pat) (rest state)))
      true `(list (list :matchfail :matchconsfail))))



(defn matchsymbol [pat state]
 (cond  (symbol? state) (list (list pat state))
        (= pat '_)  '()
        (= pat state) '()
        true (list (makebind pat state))))


(defn matchnumber [pat state]
 (cond (= pat state) '()
       true (seq (list (list :matchfail :matchnumberfail)))))


(declare genp)

(defn matchgen [pat state]
 (cond (and (genp pat) (genp state))
       (cons (makebind (first pat) (first state)) (match (third pat) (third state)))))


(defn matchempty[state]
  (cond (empty? state) ()
	true (list :matchfail)))


(defn match [pat state]
  (cond
   (symbol? pat)   
      (matchsymbol pat state)
   (empty? pat)
      (matchempty state)
   (number? pat)
      (matchnumber pat state)
   (consp pat)
      (matchcons pat state)
   (seq? pat)
      (matchlist pat state)
   (and (empty? state) (empty? pat))
      '()
   true (list :matchfail)
 ))


(defn domatch [pat state]
 (vec (reduce concat (match pat state))))

(declare member)

(defn PM [pat state]
 (matches (match pat state)))

(defn matches [binds]
  (not (member :matchfail binds)))

(defn member [item col]
 (cond (empty? col) false
       (= item (first col)) true
       true (recur item (rest col))))

(def ++ concat)

(defmacro rewrite [binds expr]
 (let [evaluated-binds (eval binds)]
   `(let ~evaluated-binds ~expr)))


(use 'clojure.stacktrace)

(defn st []
 (print-stack-trace (root-cause *e) 100))


(defn z[L]
 (let [ foo '(list 1 2)]
   (eval `(concat ~foo '~L))))

(defn add[x y]
 (+ x y))

(defn papply[func p1]
 (fn [p2] (func p1 p2)))

(defn succ[x]
 (+ x 1))

(defn enumfromlazy[params]
 (let [
       b2 (domatch '(current end) params)
       ]
   (cond  (matches b2)
          (eval (applyBinds b2
                            '(if (= current end) (list end)
                                 (cons current
                                       (enumfromlazy  (list (succ current) end)))))))))



(defn enumtolazy [end]
 (enumfromlazy (list 1 end)))

(defn enumfrom[current end]
 (if (= current end) (list end)
     (cons current (enumfrom (succ current) end))))


(defn matchfilter [pat list]
 (let [b0 (domatch pat (first list))]
   (cond
    (empty? list) '()
    (matches b0) (cons (first list) (matchfilter pat (rest list))))))


(defn enumto[end]
 (enumfrom 1 end))


(def c1 '(x (x '<- (1 2 3 4 5 6 7 8 9 10))))


(def genpat '(x <- y))

(def genempty '(x  <- ()))

(defn genhead [x]
 (cond (genp x)  (first (third x))
       true      '()))

(defn genpat [x]
 (first x))

(defn decgen [x]
 (list (first x) (second x) (rest (eval (third x)))))




(defn rewritePat[pat target val]
 (let [binds [target val]   ]
   (eval `(applyBinds ~@binds  ~pat))))


(defn emptyGen[x]
    (or (empty? (third x)) (not (seq (third x)))))
      



(defn genp [x]
 (and (= (count x) 3) (= (second x) `<-)))
(declare applyBinds)


(defn bindsfromGen[x]
 (let [pat (first x)
       source (third x)
       binds (match pat (first source))
       ]
   (cond (matches binds) binds
         true '())))

(defn consume [gen]
  (let [pat (first gen)
	expr (third gen)	
	]    
    (cond (fn? (eval (first expr))) (list (list pat (first (eval `~expr))) (list pat `<- (rest (eval `~expr))))
	  (seq? expr) (list (list pat (first expr)) (list pat `<- (rest expr)))
	  true (list :consumer :fail))))


(defn applyBind [bind expr]
  (cond (seq? expr)  (map   (fn [x] (cond (= x (first bind)) (second bind)
				    (seq? x) (applyBind bind x)
				    true x))	 expr)
        (= (first bind) expr) (second bind)))

;;(x 1) `(x x x)

(defn applyBinds [binds expr]
  (cond (empty? binds) expr
	true (applyBinds (rest binds) (applyBind (first binds) expr))))


(defmacro letbinds [binds expr]
  `(let [ ~@binds ] ~expr))

(defmacro testcmp [x y]
  `(= ~x ~y))


(defn flatbinds [x]
  (cond (empty? x) `()
	true (concat (first x) (flatbinds (rest x)))))
	
		       
(defn adder [params]
  (let [binds (match `(x (y z)) params)]
    (cond (matches params) (eval (applyBinds binds `(+ x y z))))))


(defn third [x]
  (first (next (next x))))


(defmacro msecond [x y z]
  `'~y)


(defmacro isgen [x]
  (= (eval `(msecond ~@x)) '<-))

(defmacro listcomp [[exp
		     & quals] res]
  (if  (empty? quals) `(cons ~exp ~res)
       (let [
	     q1 (first quals)
	     q  (rest quals)
	     ]
	 (if (not (eval `(isgen ~q1)))
	  `(if ~q1 (listcomp (~exp ~@q)  ~res) ~res) 
	 (let [v (first q1)  
	       l1 (third q1)  
	       h (gensym "H-")  
	       us (gensym "US-")  
	       us1  (gensym "US1-")
	       ]
	   `(letfn [(~h [ ~us ]  
			(if (empty? ~us) ~res  
			    (let [
				  ~v (first ~us)
				  ~us1 (rest ~us)  
				  ]
			      (listcomp (~exp ~@q) (~h ~us1)))))
		    ]  
	      (~h ~l1)))))))




(defn testcomp[]
  (let [s `(1 2 3)]
    (eval `(listcomp ('x ('x '<- s)) ()))))