--

add (x,y) = x + y

add1 x y = x + y



mysum [] = 0
mysum (x:xs) = x + mysum xs


mymap f [] = []
mymap f (x:xs) = f x : mymap f xs


mymap1 (f,[]) = []
mymap1 (f,x:xs) = (f x) : mymap1(f, xs)

