import Data.Char (digitToInt)

readData fname = do
    contents <- readFile fname
    return $ map (map digitToInt) (lines contents)


adjacentCoords :: [[Int]] -> (Int, Int) -> [(Int, Int)]
adjacentCoords grid (x, y) = filter isInsideGrid candidates
    where
        candidates = map (\(dx, dy) -> (x + dx, y + dy)) offsets
        isInsideGrid (x, y) = x >= 0 && y >= 0 && x < length grid && y < length (head grid)
        offsets = [(-1, 0), (-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1)]


listReplace xs i newX = take i xs ++ newX : drop (i + 1) xs


step (grid, numFlashes) = (resetGrid finalGrid, numFlashes + newFlashes)
    where
        newGrid = map (map (+1)) grid
        finalGrid = iterate updateFlashes newGrid !! 100 -- repeat to account for flash-activated flashes
        newFlashes = sum $ concatMap (map (fromEnum . (>9))) finalGrid
        resetGrid = map (map (\x -> if x > 9 then 0 else x))

        updateFlashes grid = newGrid
            where
                coords = [(x, y) | x <- [0..length grid - 1], y <- [0..length (head grid) - 1]]
                flashCoords = filter (\(x, y) -> grid !! x !! y > 9) coords
                coordsToIncrease = concatMap (adjacentCoords grid) flashCoords
                newGrid = foldl (
                        \grid (x, y) ->
                            listReplace grid x (listReplace (grid !! x) y ((grid !! x !! y) + 1))
                    ) newGrid coordsToIncrease


main = do
    grid <- readData "input.txt"
    --print $ step (grid, 0)
    print $ iterate step (grid, 0) !! 100
