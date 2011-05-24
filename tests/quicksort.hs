module Main where

filtergte (q,[]) = []
filtergte (q,(x:xs)) = 
    if q <= x then x : (filtergte (q,xs))
    else filtergte (q,xs)

filterLT (q,[]) = []
filterLT (q,(x:xs)) = 
    if q > x then [x] ++ (filterLT (q,xs))
    else filterLT (q,xs)


quicksort [] = []
quicksort (x:xs) = (quicksort (filterLT (x,xs))) 
                   ++ 
                   [x] 
                   ++ 
                   (quicksort (filtergte (x,xs)))

mytake (0,_) = []
mytake (n,x:xs) = x:mytake(n-1,xs)



fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


nth (0,x:xs) = x
nth (n,x:xs) = nth(n-1,xs)