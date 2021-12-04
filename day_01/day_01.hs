findNumIncreasing xs = sum $ map fromEnum $ map (uncurry (<)) pairs
    where pairs = zip xs (tail xs)


slidingWindows xs = zipWith3 (\x y z -> x + y + z) xs (tail xs) ((tail . tail) xs)


main = do
    contents <- readFile "input.txt"
    let numbers = map (read::String -> Int) $ lines contents
    putStrLn $ show $ (findNumIncreasing . slidingWindows) numbers 
