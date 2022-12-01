import qualified Data.List as L

parseInput :: [String] -> [[Int]]
parseInput xs = l_numeric : if length ys_filtered > 0 then parseInput ys_filtered else []
    where
        (l, ys) = span (/="") xs
        ys_filtered = dropWhile (=="") ys
        l_numeric = map (\x -> read x :: Int) l


main = do
    contents <- readFile "input.txt"
    let numbers = parseInput (lines contents)
    putStrLn $ show $ sum $ take 3 $ L.sortBy (flip compare) (map sum numbers)
