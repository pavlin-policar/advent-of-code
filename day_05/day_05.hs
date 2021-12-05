import qualified Data.Text as T
import qualified Data.Map as M

type Coord = (Int, Int)
type CoordRange = (Coord, Coord)


-- Expand a coordinate ranges into a list of all covered coordinates
expandCoordRange :: CoordRange -> [Coord]
expandCoordRange (c0, c1) = let
        (x0, y0) = c0
        (x1, y1) = c1 in
        if x1 /= x0 && y0 == y1 then expandCoordRangeX c0 c1 else
        if x0 == x1 && y1 /= y0 then expandCoordRangeY c0 c1
        else expandCoordRangeDiag c0 c1
    where expandCoordRangeX (x0, y0) (x1, y1) = [(i, y0) | i <- (generateRange x0 x1)]
          expandCoordRangeY (x0, y0) (x1, y1) = [(x0, j) | j <- (generateRange y0 y1)]
          expandCoordRangeDiag (x0, y0) (x1, y1) = zip (generateRange x0 x1) (generateRange y0 y1)


-- Generate a range working for both 1..5 and 5..1
generateRange :: Int -> Int -> [Int]
generateRange x0 x1 = [x0, (x0 + (signum diff)) .. x1] where diff = x1 - x0


ventCounts :: [Coord] -> M.Map Coord Int
ventCounts ventCoords = M.fromListWith (+) (map (\k -> (k, 1)) ventCoords)


parseCoords :: String -> [CoordRange]
parseCoords s = map (\[c1, c2] -> (c1, c2)) ventCoordTups
    where
        -- Split on ->
        ventCoordStrings = map ((T.splitOn (T.pack " -> ")) . T.pack) $ lines s
        -- Split individual coordinates
        ventCoordsSplit = map (map (T.splitOn (T.pack ","))) ventCoordStrings
        -- Convert individual coordinates to ints
        ventCoordsLst = map (map (map (\x -> (read (T.unpack x) :: Int)))) ventCoordsSplit
        -- Convert individual coords to tuples
        ventCoordTups = map (map (\[x, y] -> (x, y))) ventCoordsLst


main = do
    fContents <- readFile "input.txt"
    let ventCoordRanges = parseCoords fContents
    let ventCoords = concat $ map expandCoordRange ventCoordRanges
    let ventCounts = M.fromListWith (+) (map (\k -> (k, 1)) ventCoords)
    putStrLn $ show $ M.size $ M.filter (>1) ventCounts
