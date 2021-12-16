import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map ((!))
import Data.Maybe (mapMaybe)

readData fname = do
    contents <- readFile fname
    return $ lines contents


errorScoreTable = Map.fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

pairs = Map.fromList [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]


isCorrupt :: String -> Maybe Char
isCorrupt s = isCorrupt' s []
    where
        isCorrupt' [] stack = Nothing
        isCorrupt' (x:xs) stack =
            if Set.member x (Set.fromList ['(', '[', '{', '<']) then
                isCorrupt' xs (pairs ! x : stack)
            else
                if head stack == x then
                    isCorrupt' xs (tail stack)
                else
                    Just x


main = do
    lines <- readData "input.txt"
    print $ sum $ map (errorScoreTable !) (mapMaybe isCorrupt lines)
