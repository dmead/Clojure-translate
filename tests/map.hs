add1 x = x + 1


mymap (f, [])  = []
mymap (f, x:xs) = f x : mymap (f ,xs)



