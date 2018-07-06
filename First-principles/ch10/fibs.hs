fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

fibs20 = take 20 fibs
fibsUnder100 = takeWhile (<100) fibs

fact' = scanl (*) 1 [1..]
fact = (!!) fact'
