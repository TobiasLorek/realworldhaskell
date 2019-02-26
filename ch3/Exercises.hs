import Data.List

data Tree a = Node (Tree a) (Tree a)
            | Leaf a

data Point2 = Point2 Double Double

cross :: Point2 -> Point2 -> Double
cross (Point2 x1 y1) (Point2 x2 y2) = x1 * x2 + y1 * y2

sub :: Point2 -> Point2 -> Point2
sub (Point2 x1 y1) (Point2 x2 y2) = Point2 (x1 - x2) (y1 - y2)

data Direction = Left | Right | Straight

turn :: Point2 -> Point2 -> Point2 -> Direction
height :: Tree a -> Int
height (Leaf a) = 0
height (Node t1 t2) = 1 + max (height t1) (height t2)

myLength :: [a] -> Integer
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

sortList :: [[a]] -> [[a]]
sortList = sortBy (\x y -> compare (length x) (length y))

intersperse :: a -> [[a]] -> [a]
intersperse i [] = []
intersperse i (x:[]) = x
intersperse i xs = foldr1 (\acc item -> item ++ i:acc) xs
