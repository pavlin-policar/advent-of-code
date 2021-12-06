import qualified Data.Map as M


type StateCounts = M.Map Int Int


-- Split string on delimiter
splitOnDelim :: Char -> String -> [String]
splitOnDelim d s = case dropWhile (== d) s of
    "" -> []
    s' -> w : (splitOnDelim d ws) where (w, ws) = break (== d) s'


readData :: FilePath -> IO [Int]
readData fname = do
    contents <- readFile fname
    return $ map (\n -> read n :: Int) $ splitOnDelim ',' $ (head . lines) contents


step :: [Int] -> [Int]
step state = resetAdults $ newState ++ (offspring state)
    where newState = map (subtract 1) state
          offspring = foldl (\acc x -> if x == 0 then 8 : acc else acc) []
          resetAdults = map (\x -> if x < 0 then 6 else x)


stateCounts :: [Int] -> StateCounts
stateCounts state = M.fromListWith (+) (map (\k -> (k, 1)) state)


stepEfficient :: StateCounts -> StateCounts
stepEfficient state = M.fromListWith (+) $ ((resetAdults . decAdults) state) ++ offspring
    where
        decAdults = map (\(k, v) -> (k - 1, v)) . M.toList
        resetAdults = map (\(k, v) -> if k < 0 then (6, v) else (k, v))
        offspring = case M.lookup 0 state of
            Nothing -> []
            Just x -> [(8, x)]


main = do
    initialState <- readData "input.txt"
    -- Part 1
    --putStrLn $ show $ length $ iterate step initialState !! 80
    -- Part 2
    putStrLn $ show $ M.foldl (+) 0 $ iterate stepEfficient (stateCounts initialState) !! 256
