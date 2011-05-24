test x = do y <- do x <- return $ x + x 
                    return $ x + 1
            return y