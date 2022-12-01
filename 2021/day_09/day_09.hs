import Data.Char (digitToInt)
import qualified Data.Set as Set
import qualified Data.List as L


readData fname = do
    contents <- readFile fname
    return $ map (map digitToInt) (lines contents)


adjacentCoordOffset = [(-1, 0), (0, -1), (1, 0), (0, 1)]


-- Generate a list of valid adjacent coordinates
adjacentCoords heightMap (x, y) = filter (
        \(x, y) -> x >= 0 && y >= 0 && x < length heightMap && y < length (head heightMap)
    ) candidates
    where
        candidates = map (\(dx, dy) -> (x + dx, y + dy)) adjacentCoordOffset


findLowest heightMap (x, y) = all ((==True) . (>heightMap !! x !! y)) adjacentHeights
    where
        adjacentHeights = map (\(x, y) -> heightMap !! x !! y) (adjacentCoords heightMap (x, y))


riskLevels heightMap = map (\(x, y) -> (heightMap !! x !! y) + 1)


expandBasin :: [[Int]] -> (Int, Int) -> Set.Set (Int, Int)
expandBasin heightMap coords = expandBasin' Set.empty coords
    where
        expandBasin' :: Set.Set (Int, Int) -> (Int, Int) -> Set.Set (Int, Int)
        expandBasin' currBasin coords = if currVal == 9 then currBasin
            else foldl expandBasin' newBasin unseenNeighbors
            where
                currVal = heightMap !! fst coords !! snd coords
                newBasin = Set.insert coords currBasin
                unseenNeighbors = filter (`Set.notMember` newBasin) (adjacentCoords heightMap coords)


main = do
    heightMap <- readData "input.txt"
    let coords = [(x, y) | x <- [0..length heightMap - 1], y <- [0..length (head heightMap) - 1]]
    let minCoords = map snd $ filter fst (zip (map (findLowest heightMap) coords) coords)
    -- Part 1
    print $ sum $ riskLevels heightMap minCoords
    -- Part 2
    let basins = map (expandBasin heightMap) minCoords
    let basinSizes = map Set.size basins
    let topThree = take 3 (L.sortBy (flip compare) basinSizes)
    print $ product topThree
