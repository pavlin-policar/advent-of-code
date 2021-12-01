findNumIncreasing xs = sum $ map fromEnum $ map (\(x, y) -> x < y) pairs
    where pairs = zip xs (tail xs)


slidingWindows xs = map (\(x, y, z) -> x + y + z) triplets
    where triplets = zip3 xs (tail xs) ((tail . tail) xs)

{-
main =
    putStrLn $ show $ findNumIncreasing [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
-}


main = do
    contents <- readFile "input.txt"
    --ls_ints <- convertToIntList contents
    putStrLn $ show $ (findNumIncreasing . slidingWindows) $ map (read::String -> Int) $ lines contents
