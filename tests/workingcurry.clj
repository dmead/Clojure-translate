(defn add3[params]
  (let [ 
    b0  (match `x params) ;; bind x
        ]
    (cond 
     (matches b0)
     (eval
      (applyBinds b0
		  (let [param (gensym "l")
			binds (gensym "b")]
		    `(fn [~param]			    
		       (let [~binds (match `y ~param)] ;; bind y
			 (cond (matches ~binds )
			       (eval (applyBinds ~binds
						 `(fn [~param]
						    (let [~binds (matches `y ~param)]  ;; bind z
						      (cond
						       (matches ~binds)
						       (eval (applyBinds ~binds  '(+ (+ x y) z))) )))))))))))
     
     true (list :patternmatchfail add3 params) )))





(defn add3[params]
  (let [ 
    b0  (match `x params) ;; bind x
        ]
    (cond 
     (matches b0)
     (eval     (applyBinds b0
	     (let [param (gensym "l")
		   binds (gensym "b1")]
	      `(fn [~param]
		(let [~binds  (match `y ~param)] ;; bind y
		  (cond (matches ~binds)
	 		(eval (applyBinds ~binds
			       '(fn [~param]
				 (let [~binds (match `z ~param)]

				   (cond (matches ~binds)
					 (eval (applyBinds ~binds
						   '(fn [~param]
						      (let [~binds (match `w ~param)]
							(cond (matches ~binds)
							      (eval (applyBinds ~binds `(+ x y z w))))))))))))))))))))))
     


