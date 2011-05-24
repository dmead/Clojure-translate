myflip [] = []
myflip [x] = [x]
myflip (x:y:xs) = y:x:myflip xs