
xs = ["1000","2000","3000","","4000","","5000","6000","","7000","8000","9000","","10000"]


parseInput :: [String] -> [[Int]]
parseInput xs = l_numeric : if length ys_filtered > 0 then parseInput ys_filtered else []
    where
        (l, ys) = span (/="") xs
        ys_filtered = dropWhile (=="") ys
        l_numeric = map (\x -> read x :: Int) l


main = do
    contents <- readFile "input.txt"
    let numbers = parseInput (lines contents)
    putStrLn $ show $ maximum $ map sum numbers
