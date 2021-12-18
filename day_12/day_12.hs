import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Char as Char


readGraph fname = do
    contents <- readFile fname
    let ls = map (map T.strip . T.splitOn (T.pack "-") . T.pack) $ lines contents
    return $ map ((\[x, y] -> (x, y)) . map T.unpack) ls


type AdjacencyList = M.Map String [String]

-- Convert an edge list to an adjacency list
edges2adj :: [(String, String)] -> AdjacencyList
edges2adj edges = M.fromListWith (++) (map (\(x, y) -> (x, [y])) edges)


generatePaths :: AdjacencyList -> String -> String -> [[String]]
generatePaths graph source target = generatePaths' Set.empty [[]] source
    where
        generatePaths' :: Set.Set String -> [[String]] -> String -> [[String]]
        generatePaths' disallowed paths source
            | source == target = addToPaths paths source
            | null children = [[]]
            | otherwise = concatMap (generatePaths' newDisallowed newPaths) children
            where
                children = filter (\x -> not (Set.member x disallowed)) $ M.findWithDefault [] source graph
                newDisallowed = if Char.isLower (head source) then Set.insert source disallowed else disallowed
                newPaths = addToPaths paths source


addToPaths :: [[a]] -> a -> [[a]]
addToPaths paths node = map (\t -> t ++ [node]) paths

main = do
    edges <- readGraph "input.txt"
    let undirectedEdges = edges ++ map (\(x, y) -> (y, x)) edges
    let graph = edges2adj undirectedEdges
    let paths = filter (not . null) (generatePaths graph "start" "end")
    mapM_ print paths
    print $ length paths
