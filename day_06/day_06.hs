
newtype Days = Days Int


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


main = do
    initialState <- readData "input.txt"
    putStrLn $ show $ length $ iterate step initialState !! 80
