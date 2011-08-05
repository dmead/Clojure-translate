add x y = case x of 
            0 -> case y of
                   0 -> 2
                   y -> 6
            x -> x + y
                   
-- x y =  x + y


add1  (x,y) = x + y


--main :: IO ()
--main = 
--    do 
 --     y <- readLn
 --     x <- readLn
  --    return ()
