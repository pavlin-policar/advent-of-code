import qualified Data.Map as M
import Data.Char (isSpace)
import Data.List (transpose)

data Move = Move String String deriving (Show)
type CrateMap = M.Map String [Char]


chunkList :: Int -> [a] -> [[a]]
chunkList n [] = []
chunkList n xs = take n xs : chunkList n (drop n xs)

-- Parse input
parseInput :: [String] -> (CrateMap, [Move])
parseInput input =
    let (crates, moves) = span (/= "") input
    in (parseCrates crates, concatMap parseMove (dropWhile (== "") moves))

parseCrates :: [String] -> CrateMap
parseCrates crateString = M.fromListWith (++) (zip queueNames filteredCrates)
    where
        parts = map (map init . chunkList 4) crateString
        queueNames = map (filter (not . isSpace)) (last parts)
        crateStringList = transpose $ map (map (!! 1)) (init parts)
        filteredCrates = map (dropWhile (== ' ')) crateStringList

parseMove :: String -> [Move]
parseMove moveString = generateMoves (wordParts !! 3) (wordParts !! 5) numCrates
    where
        wordParts = words moveString
        numCrates = read (wordParts !! 1)::Int

        generateMoves from to 1 = [Move from to]
        generateMoves from to n = Move from to : generateMoves from to (n - 1)


-- Solve puzzle
applyMoves :: CrateMap -> [Move] -> CrateMap
applyMoves crates moves = foldl applyMove crates moves
    where
        applyMove :: CrateMap -> Move -> CrateMap
        applyMove crates (Move from to) = M.insert from newFromCrates (M.insert to newToCrates crates)
            where
                itemToMove = head $ crates M.! from
                newFromCrates = tail $ crates M.! from
                newToCrates = let curr = crates M.! to in itemToMove : curr

main = do
    input <- readFile "input.txt"
    let (crates, moves) = parseInput (lines input)
    print $ M.foldr (\x acc -> head x : acc) [] (applyMoves crates moves)
