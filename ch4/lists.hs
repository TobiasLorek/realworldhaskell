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
myconcat (a:as) = a ++ myconcat as


myreverse :: [a] -> [a]
myreverse [] = []
myreverse (a:as) = myreverse as ++ [a]

mand :: [Bool] -> Bool
mand [] = True
mand (x:xs) = x && mand xs

mall :: (a -> Bool) -> [a] -> Bool
mall f [] = True
mall f (x:xs) = f x && mall f xs

many :: (a -> Bool) -> [a] -> Bool
many f [] = False
many f (x:xs) = f x || many f xs

mtake :: Int -> [a] -> [a]
mtake i [] = []
mtake 0 x  = []
mtake i (x:xs) = x : mtake (i -1) xs

mdrop :: Int -> [a] -> [a]
mdrop i [] = []
mdrop 0 x  = x
mdrop i (x:xs) = mdrop (i -1) xs

msplitAt :: Int -> [a] -> ([a], [a])
msplitAt 0 x = ([], x)
msplitAt i [] = ([], [])
msplitAt i x = (myreverse p1, p2)
    where 
        walk 0 (x, y)      = (x, y)
        walk i (x, [])     = (x, [])
        walk i (x, y:ys)  = (y:x, ys)
        (p1, p2) = walk i ([], x)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead x = Just (myhead x)

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail x = Just (mytail x)

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast x = Just (mylast x)

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit x = Just (myinit x)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith prd lst = walk prd lst []
    where walk prd [] acc = acc
          walk prd (l:ls) [] | prd l = walk prd ls []
                             | otherwise = walk prd ls [[l]] 
          walk prd (l:ls) (a:as) | prd l = walk prd ls ([]:a:as)
                                 | otherwise = walk prd ls ((l:a):as)

