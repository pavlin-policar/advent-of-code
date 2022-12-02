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


main :: IO ()
main = do
    contents <- readFile "input.txt"
    let hands = map (tuplify2 . map mapCharToHand . words) (lines contents)
    let scores_unpacked = zip (map (uncurry evalHand) hands) (map snd hands)
    let result = map (\(x, y) -> scoreOutcome x + scoreHand y) scores_unpacked
    putStrLn $ show $ sum result
