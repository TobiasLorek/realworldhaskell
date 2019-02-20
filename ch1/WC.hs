
counts input = (clines input) ++ " " ++ (cwords input) ++ " " ++ (cchars input) ++ "\n"
    where clines = show . length . lines 
          cwords = show . length . words 
          cchars = show . length

main = interact counts 
        
    
