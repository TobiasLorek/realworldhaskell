import Data.List
import Data.Char

type ErrorMessage = String

asInt_Fold str = fst $ foldr step (0, 0) str
    where step next (acc, ind) | next == '+' = (acc, 0)
                               | next == '-' = (-acc, 0)
                               | otherwise = ((digitToInt next) * 10 ^ ind + acc, ind + 1)

asInt_Either :: String -> Either ErrorMessage Int
asInt_Either str = fst $ foldr step ((Right 0), 0) str
    where plus  (acc, _)              = (acc, 0)
          minus ((Right acc), _)      = (Right $ -acc, 0)
          add   next (Right acc, exp) = (Right $ next * 10 ^ exp + acc, exp + 1)
          notInt _                    = (Left "Not an int", 0)
          
          step next | next == '+'  = plus
                    | next == '-'  = minus
                    | isDigit next = add $ digitToInt next
                    | otherwise    = notInt

myconcat :: [[a]] -> [a]
myconcat lists = foldr (++) [] lists

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ []                    = []
myTakeWhile pred (x:xs) | pred x    = x : myTakeWhile xs
myTakeWhile pred (x:xs) | otherwise = []

myTakeWhileF :: (a -> Bool) -> [a] -> [a]
myTakeWhileF pred xs = foldr step [] xs
    where step x ys | pred x    = x:ys
                    | otherwise = ys

myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
mygroupBy bipred list = foldr step [] list
    where update gr x   | bipred (head gr) x = x:gr
                        | otherwise          = gr
          step x []     = [[x]]
          step x groups = foldr update [] groups
