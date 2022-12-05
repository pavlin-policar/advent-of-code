import qualified Data.Map as M
import Data.Char (isSpace)
import Data.List (transpose)

data Move = Move String String Int deriving (Show)
type CrateMap = M.Map String [Char]


chunkList :: Int -> [a] -> [[a]]
chunkList n [] = []
chunkList n xs = take n xs : chunkList n (drop n xs)

-- Parse input
parseInput :: [String] -> (CrateMap, [Move])
parseInput input =
    let (crates, moves) = span (/= "") input
    in (parseCrates crates, map parseMove (dropWhile (== "") moves))

parseCrates :: [String] -> CrateMap
parseCrates crateString = M.fromListWith (++) (zip queueNames filteredCrates)
    where
        parts = map (map init . chunkList 4) crateString
        queueNames = map (filter (not . isSpace)) (last parts)
        crateStringList = transpose $ map (map (!! 1)) (init parts)
        filteredCrates = map (dropWhile (== ' ')) crateStringList

parseMove :: String -> Move
parseMove moveString = Move (wordParts !! 3) (wordParts !! 5) numCrates
    where
        wordParts = words moveString
        numCrates = read (wordParts !! 1)::Int


-- Solve puzzle
applyMoves :: CrateMap -> [Move] -> CrateMap
applyMoves crates moves = foldl applyMove crates moves
    where
        applyMove :: CrateMap -> Move -> CrateMap
        applyMove crates (Move from to numCrates) = M.insert from newFromCrates (M.insert to newToCrates crates)
            where
                itemToMove = take numCrates (crates M.! from)
                newFromCrates = drop numCrates (crates M.! from)
                newToCrates = let curr = crates M.! to in itemToMove ++ curr

main = do
    input <- readFile "input.txt"
    let (crates, moves) = parseInput (lines input)
    print $ M.foldr (\x acc -> head x : acc) [] (applyMoves crates moves)
