import System.Environment (getArgs)

interactWith function inputFile outputFile = do
    input <- readFile inputFile
    putStr input
    writeFile outputFile (function input)

accumulateTransposed :: [String] -> Int -> [String] -> [String]
accumulateTransposed [] lineNo transposed = transposed
accumulateTransposed (l:ls) lineNo trans = accumulateTransposed ls (lineNo + 1) (transposeLine l lineNo trans)

transposeLine :: String -> Int -> [String] -> [String]
transposeLine [] i [] = []
transposeLine (c:cs) i [] = [c] : transposeLine cs i []
transposeLine [] i (l:ls) = (add " " i l " ") : transposeLine [] i ls
transposeLine (c:cs) i (l:ls) = (add c i l " ") : transposeLine cs i ls

add :: a -> Int -> [a] -> a -> [a]
add x 0 xs f = x:xs
add x n [] f = f : add x (n - 1) [] f
add x n (y:ys) f = y : add x (n - 1) ys f

main = mainWith myFunc
    where mainWith func = do
            args <- getArgs
            case args of 
              [input, output] -> interactWith func input output 
              _               -> putStrLn "need two filenames man"
          myFunc = toFirstWords
