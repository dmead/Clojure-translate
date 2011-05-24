module Hello where



data What = Foo | Bar


test x | x > 1 = return "hello"

main = do x <- test 1
          print "hello"
          return ()