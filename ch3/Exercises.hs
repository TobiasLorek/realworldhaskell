myLength :: [a] -> Integer
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

--myLength2 = foldr (\ x = (+) 1_

