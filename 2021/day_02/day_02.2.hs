type HorizontalPosition = Int
type Depth = Int
type Aim = Int
data Position = Position HorizontalPosition Depth Aim

data Action = Forward Int | Down Int | Up Int


move :: Position -> Action -> Position
move (Position x0 z0 a0) (Forward x) = Position (x0 + x) (z0 + a0 * x) a0
move (Position x0 z0 a0) (Down a) = Position x0 z0 (a0 + a)
move (Position x0 z0 a0) (Up a) = Position x0 z0 (a0 - a)


prodPosition :: Position -> Int
prodPosition (Position x z a) = x * z


parse :: [String] -> Action
parse ["forward", amount] = Forward (read amount :: Int)
parse ["down", amount] = Down (read amount :: Int)
parse ["up", amount] = Up (read amount :: Int)


main = do
    contents <- readFile "input.txt"
    let steps = map (parse . words) $ lines contents
    let initialPosition = Position 0 0 0
    putStrLn $ show $ prodPosition (foldl move initialPosition steps)
