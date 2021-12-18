import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Char as Char
import Data.Map ((!))


readGraph fname = do
    contents <- readFile fname
    let ls = map (map T.strip . T.splitOn (T.pack "-") . T.pack) $ lines contents
    return $ map ((\[x, y] -> (x, y)) . map T.unpack) ls


type AdjacencyList = M.Map String [String]

-- Convert an edge list to an adjacency list
edges2adj :: [(String, String)] -> AdjacencyList
edges2adj edges = M.fromListWith (++) (map (\(x, y) -> (x, [y])) edges)


-- Keep track of number of times we visit nodes
type StateDict =  M.Map String Int

isAllowed :: StateDict -> String -> Bool
isAllowed stateDict node
    | node == "start" || node == "stop" = M.findWithDefault 0 node stateDict < 1
    | Char.isUpper (head node) = True
    | otherwise = M.findWithDefault 0 node stateDict < 1

updateStateDict :: String -> StateDict -> StateDict
updateStateDict node = M.insertWith (+) node 1


-- Add a node to the end of a list of paths e.g. add 9 to [[1, 2], [3, 4]] -> [[1, 2, 9], [3, 4, 9]]
addToPaths :: [[a]] -> a -> [[a]]
addToPaths paths node = map (\t -> t ++ [node]) paths


-- Generate paths through the graph
generatePaths :: AdjacencyList -> String -> String -> [[String]]
generatePaths graph source target = generatePaths' M.empty [[]] source
    where
        generatePaths' :: StateDict -> [[String]] -> String -> [[String]]
        generatePaths' state paths source
            | source == target = addToPaths paths source
            | null children = [[]]
            | otherwise = concatMap (generatePaths' (updateStateDict source state) newPaths) children
            where
                children = filter (isAllowed state) (M.findWithDefault [] source graph)
                newPaths = addToPaths paths source


main = do
    edges <- readGraph "input.txt"
    let undirectedEdges = edges ++ map (\(x, y) -> (y, x)) edges
    let graph = edges2adj undirectedEdges
    let paths = filter (not . null) (generatePaths graph "start" "end")
    mapM_ print paths
    print $ length paths
