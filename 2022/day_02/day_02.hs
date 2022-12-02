data Move = Rock | Paper | Scissors deriving (Eq, Show)
data Outcome = Win | Draw | Lose deriving (Show)


scoreOutcome :: Outcome -> Int
scoreOutcome Win = 6
scoreOutcome Draw = 3
scoreOutcome Lose = 0


scoreHand :: Move -> Int
scoreHand Rock = 1
scoreHand Paper = 2
scoreHand Scissors = 3


beats :: Move -> Move
beats move = case move of
    Rock -> Scissors
    Paper -> Rock
    Scissors -> Paper


evalHand :: Move -> Move -> Outcome
evalHand my_move opposing_move
    | my_move == beats opposing_move = Win
    | my_move == opposing_move = Draw
    | otherwise = Lose


mapCharToHand :: String -> Move
mapCharToHand "X" = Rock
mapCharToHand "Y" = Paper
mapCharToHand "Z" = Scissors
mapCharToHand "A" = Rock
mapCharToHand "B" = Paper
mapCharToHand "C" = Scissors


tuplify2 :: [a] -> (a, a)
tuplify2 [x, y] = (x, y)


pt1 :: [(Move, Move)] -> [Int]
pt1 hands = map (\(x, y) -> scoreOutcome x + scoreHand y) scores_unpacked
    where
        scores_unpacked = zip (map (uncurry evalHand) hands) (map snd hands)


-- pt. 2
mapCharToOutcome :: String -> Outcome
mapCharToOutcome "X" = Lose
mapCharToOutcome "Y" = Draw
mapCharToOutcome "Z" = Win


pt2 :: [(String, String)] -> [(Move, Move)]
pt2 = map (\(x, y) -> let m = mapCharToHand x in (m, determine_move m (mapCharToOutcome y)))
    where
        determine_move move Draw = move
        determine_move move Lose = beats move
        determine_move move Win = (beats . beats) move


main :: IO ()
main = do
    contents <- readFile "input.txt"
    --let result = pt1 $ map (tuplify2 . map mapCharToHand . words) (lines contents)
    let result = sum $ pt1 $ pt2 $ map (tuplify2 . words) (lines contents)
    putStrLn $ show result
