{-
test comprehensions for translation
-}

--we needd a non curried zip
tzip ([],[]) = []
tzip (x:xs,y:ys) = (x,y) : tzip(xs,ys)

--1. simple stuff

listid n = [x | x <- n]

squareall l  = [ x * x | x <- l]


--2. quicksort
qs []    = []
qs (h:t) = qs[x | x<-t, x<=h] ++ h:qs[x | x<-t, x>h]



 --3. sort from the peyton-jones book, nearly the same
sort [] = []
sort (x:xs)  = sort [y | y <- xs, y < x] 
               ++ [x]++
               sort [y | y <- xs, y >= x]



--4. vector addition, also from the book
vecAdd (xs,ys) = [x + y | (x,y) <- tzip (xs,ys)]


--5. singletons filters a list of ints for the singleton lists
singletons xs = [ x | [x] <- xs]



--6. example from the langauge definition
c1 _ =[ x |  xs   <- [ [(1,2),(3,4)], [(5,4),(3,2)] ], 
                      (3,x) <- xs ]

--7. cartesian product
cartesian (s1,s2) =  [(x, y) | x <- s1, y <- s2]



--8. fibinacci sequence  from the langauge tutorials
--fibs _  = 0 : 1 : [a + b | (a,b) <- tzip (fibs 0 ,(tail fibs ))]
