t1 q = [x | x <-[1,2,3,4,5,6,7,8,9,10]]


myblock z = do x <- do x <- return [1..z]
                       print x
                       x <- return 1
                       return x
               return x
