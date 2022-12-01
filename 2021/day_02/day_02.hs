type HorizontalPosition = Integer
type Depth = Integer
data Position = Position HorizontalPosition Depth

data Action = Forward Integer | Down Integer | Up Integer


move :: Position -> Action -> Position
move (Position x0 z0) (Forward x) = Position (x0 + x) z0
move (Position x0 z0) (Down z) = Position x0 (z0 + z)
move (Position x0 z0) (Up z) = Position x0 (z0 - z)


prodPosition :: Position -> Integer
prodPosition (Position x z) = x * z


parse :: [String] -> Action
parse ["forward", amount] = Forward (read amount :: Integer)
parse ["down", amount] = Down (read amount :: Integer)
parse ["up", amount] = Up (read amount :: Integer)


main = do
    contents <- readFile "input.txt"
    let steps = map (parse . words) $ lines contents
    let initialPosition = Position 0 0
    putStrLn $ show $ prodPosition (foldl move initialPosition steps)
