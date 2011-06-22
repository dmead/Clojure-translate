(defn addcurry [param]
  (let [b0 (match 'x param)]
    (cond
     (matches b0)
     (eval (applyBinds b0
		       '(fn [p2] (let [b0 (match 'y p2)]
						   (+ x y)))))			   
     true (list :fail addcurry param)))) 