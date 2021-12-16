import Data.Char (digitToInt)


readData fname = do
    contents <- readFile fname
    return $ map (map digitToInt) (lines contents)


adjacentCoordOffset = [(-1, 0), (0, -1), (1, 0), (0, 1)]


checkAdjacent heightMap (x, y) = all ((==True) . (>heightMap !! x !! y)) adjacentHeights
    where
        candidates = map (\(dx, dy) -> (x + dx, y + dy)) adjacentCoordOffset
        filteredCandidates = filter (\(x, y) -> x >= 0 && y >= 0 && x < length heightMap && y < length (heightMap !! 0)) candidates
        adjacentHeights = map (\(x, y) -> heightMap !! x !! y) filteredCandidates


riskLevels heightMap = map (\(x, y) -> (heightMap !! x !! y) + 1)


main = do
    heightMap <- readData "input.txt"
    let coords = [(x, y) | x <- [0..length heightMap - 1], y <- [0..length (heightMap !! 1) - 1]]
    let minCoords = map snd $ filter fst (zip (map (checkAdjacent heightMap) coords) coords)
    print $ sum $ riskLevels heightMap minCoords
