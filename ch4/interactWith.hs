import System.Environment (getArgs)

interactWith function inputFile outputFile = do
    input <- readFile inputFile
    putStr input
    writeFile outputFile (function input)

firstWords :: [String] -> [String]
firstWords [] = []
firstWords (l:ls) = frst l:firstWords ls
    where frst "" = ""
          frst l = head $ words l

toFirstWords :: String -> String
toFirstWords file = unlines $ firstWords $ lines file

main = mainWith myFunc
    where mainWith func = do
            args <- getArgs
            case args of 
              [input, output] -> interactWith func input output 
              _               -> putStrLn "need two filenames man"
          myFunc = toFirstWords
