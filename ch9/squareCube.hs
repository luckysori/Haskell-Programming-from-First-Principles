mySqr = [x^2 | x <- [1..5]]
myCube = [x^3 | x <- [1..5]]

myTuples = [(x, y) | x <- mySqr, y <- myCube]
myTuplesAlt = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]
myTuplesAltLength = length myTuplesAlt
