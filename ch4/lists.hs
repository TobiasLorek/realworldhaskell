-- list functions

mylength :: [a] -> Integer
mylength [] = 0
mylength (x:xs) = 1 + mylength xs

mynull :: [a] -> Bool
mynull [] = True
mynull xs = False

myhead :: [a] -> a
myhead (a:as) = a

mytail :: [a] -> [a]
mytail [] = []
mytail (a:as) = as

mylast :: [a] -> a
mylast [a] = a
mylast (a:as) = mylast as

myinit :: [a] -> [a]
myinit [a] = []
myinit (a:as) = a : myinit as

(++++) :: [a] -> [a] -> [a]
(++++) [] xs = xs
(++++) (y:ys) xs = y : (ys ++++ xs)

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat [[]] = []
myconcat [a:as] = a ++ myconcat as


myreverse :: [a] -> [a]
myreverse [] = []
myreverse (a:as) = myreverse as ++ [a]

mand :: [Bool] -> Bool
mand [] = True
mand (x:xs) = x && mand xs

