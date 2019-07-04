import System.Environment (getArgs)
import Data.Maybe

interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function input)

maybeLine :: String -> [Maybe Char]
maybeLine line = (map Just line) ++ (repeat Nothing)

transposeLists :: [[a]] -> [[a]]
transposeLists = foldr (zipWith (:)) (repeat [])

transposeToMaybe :: [String] -> [[Maybe Char]]
transposeToMaybe lines = takeWhile (any isJust) $ transposeLists $ map maybeLine lines

transposeText :: String -> String
transposeText text = unlines $ map (map (fromMaybe ' ')) $ transposeToMaybe $ lines text

main = mainWith myFunc
    where mainWith func = do
            args <- getArgs
            case args of 
              [input, output] -> interactWith func input output 
              _               -> putStrLn "need two filenames man"
          myFunc = transposeText
