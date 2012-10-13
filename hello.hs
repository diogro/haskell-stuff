main = do
        putStrLn "Qualque e a base?"
        base <- getLine
        putStrLn "Qualque e a altura?"
        altura <- getLine
        putStrLn ("Area deu " ++show (area (read base) (read altura))++".")
        
area :: Int -> Int -> Int
area x y = x*y
