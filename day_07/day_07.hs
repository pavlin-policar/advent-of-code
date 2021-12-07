-- Split string on delimiter
splitOnDelim :: Char -> String -> [String]
splitOnDelim d s = case dropWhile (== d) s of
    "" -> []
    s' -> w : splitOnDelim d ws where (w, ws) = break (== d) s'


readData :: FilePath -> IO [Int]
readData fname = do
    contents <- readFile fname
    return $ map (\n -> read n :: Int) $ splitOnDelim ',' $ (head . lines) contents


findMinCost :: [Int] -> (Int, Int)
findMinCost positions = foldl1 (\t0 t1 -> if snd t1 < snd t0 then t1 else t0) costs
    where
        candidatePositions = [(minimum positions) .. (maximum positions)]
        costs = map (\p -> (p, sum (calculateCost p positions))) candidatePositions
        calculateCost val = map (abs . subtract val)


main = do
    positions <- readData "input.txt"
    putStrLn $ show $ findMinCost positions
