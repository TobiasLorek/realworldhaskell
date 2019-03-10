import System.Environment (getArgs)

interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function input)

main = mainWith myFunc
    where mainWith func = do
            args <- getArgs
            case args of 
              [input, output] -> interactWith func input output 
              _               -> putStrLn "need two filenames man"
          myFunc = id
            
