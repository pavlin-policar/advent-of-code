wordsWhen :: (Char -> Bool) -> [Char] -> [[Char]]
wordsWhen p s = 
    case dropWhile p s of
        "" -> []
        s' -> w : wordsWhen p s''
            where (w, s'') = break p s'


parseLine :: [Char] -> ((Int, Int), (Int, Int))
parseLine l = tuple_coords
    where
        line_parts = wordsWhen (==',') l
        line_coords = map (wordsWhen (=='-')) line_parts
        numeric_coords = map (map (read::String->Int)) line_coords
        tuple_coords = tuplify2 $ map tuplify2 numeric_coords


tuplify2 :: [b] -> (b, b)
tuplify2 [x,y] = (x, y)


solvePt1 :: (Int, Int) -> (Int, Int) -> Bool
solvePt1 (i, j) (k, l) = (i >= k && j <= l) || (k >= i && l <= j)


solvePt2 :: (Int, Int) -> (Int, Int) -> Bool
solvePt2 (i, j) (k, l) = (j >= k && j <= l) || (i >= k && i <= l) || (i <= k && j >= l) || (k <= i && l >= j)


main :: IO ()
main = do
    contents <- readFile "input.txt"
    let input = lines contents
    let coords = map parseLine input

    putStrLn $ show $ sum (map (fromEnum . uncurry solvePt2) coords)
