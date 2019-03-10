import Data.List

data Tree a = Node (Tree a) (Tree a)
            | Leaf a 
            deriving Show

data P2 = P2 Double Double deriving (Show, Eq)

data Direction = LeftTurn | RightTurn | Straight deriving Show

dirToOrder :: Direction -> Ordering
dirToOrder dir = case dir of
    LeftTurn  -> LT
    Straight  -> EQ
    RightTurn -> GT

turn :: P2 -> P2 -> P2 -> Direction
turn p1 p2 p3
    | z == 0 = Straight
    | z <  0 = RightTurn
    | z >  0 = LeftTurn
    where zdir (P2 x1 y1) (P2 x2 y2) (P2 x3 y3) 
            = (x2 - x1) * (y3 - y2) - (y2 - y1) * (x3 - x2)
          z = zdir p1 p2 p3


turns :: [P2] -> [Direction]
turns (x:y:z:ps) = turn x y z : turns (y:z:ps)
turns ps = []

p2min :: P2 -> P2 -> Ordering
p2min (P2 x1 y1) (P2 x2 y2) | x1 < x2              = LT
                            | x1 == x2 && y1 < y2  = LT
                            | x1 == x2 && y1 == y2 = EQ
                            | otherwise            = GT

points = [P2 0 0, P2 1 0, P2 0.5 0.5, P2 0 1]

compPoints :: P2 -> P2 -> P2 -> Ordering
compPoints origin p1 p2 = dirToOrder (turn origin p1 p2)

grahamScan :: [P2] -> [P2]
grahamScan [] = []
grahamScan [p] = [p]
grahamScan [p1, p2] = [p1, p2]
grahamScan [p1, p2, p3] = [p1, p2, p3]
grahamScan (p:ps) = foldl (\x y -> reduce (y:x)) [start] sortedPoints
    where reduce (pn2:pn1:pn:ps) = case turn pn pn1 pn2 of
            RightTurn -> reduce $ pn2:pn:ps
            _         -> pn2:pn1:pn:ps
          reduce ps = ps
          start = minimumBy p2min ps
          sortedPoints = sortBy (compPoints start) (filter (/= start) ps)
            

height :: Tree a -> Int
height (Leaf a) = 0
height (Node t1 t2) = 1 + max (height t1) (height t2)

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
isPalindrom xs = and $ map (\(x, y) -> x == y) $ zip xs (reverse xs)


sortList :: [[a]] -> [[a]]
sortList = sortBy (\x y -> compare (length x) (length y))

intersperse :: a -> [[a]] -> [a]
intersperse i [] = []
intersperse i (x:[]) = x
intersperse i xs = foldr1 (\acc item -> item ++ i:acc) xs

main = print $ show $ myLength2 [1,2,3]
