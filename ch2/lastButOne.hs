lastButOne :: [a] -> a
lastButOne [x, y] = x
lastButOne (x:y:z:xs) = lastButOne (y:z:xs)

main = print $ show $ lastButOne [1,2,3,4,5,6]

