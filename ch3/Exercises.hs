myLength :: [a] -> Integer
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myLength2 = foldr (\x -> (+) 1) 0
mean :: [Double] -> Double
mean xs = foldl reducer 0 zippedXs
    where reducer = (\acc v -> ((fst v - 1) * acc + snd v) / fst v)
          zippedXs = zip [1.0..] xs

mean2 xs = fst val / snd val
    where val = foldl (\x y -> (fst x + y, snd x + 1)) (0.0, 0.0) xs

toPalindrom :: [a] -> [a]
toPalindrom xs = xs ++ reverse xs

isPalindrom :: Eq a => [a] -> Bool
isPalindrom xs = and (foldl (\x y -> x == y) True zip xs (reverse xs))

main = print $ show $ myLength2 [1,2,3]

