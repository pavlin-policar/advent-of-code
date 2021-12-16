import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map ((!))
import Data.Maybe (mapMaybe)
import qualified Data.List as List

readData fname = do
    contents <- readFile fname
    return $ lines contents


errorScoreTable = Map.fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]
autoCompleteScoreTable = Map.fromList [(')', 1), (']', 2), ('}', 3), ('>', 4)]

pairs = Map.fromList [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]


data CharStatus = Ok | Illegal Char | Missing String deriving (Show)


evalString :: String -> CharStatus
evalString s = evalString' s []
    where
        evalString' [] stack = if null stack then Ok else Missing stack
        evalString' (x:xs) stack
          | Set.member x (Set.fromList ['(', '[', '{', '<']) = evalString' xs (pairs ! x : stack)
          | head stack == x = evalString' xs (tail stack)
          | otherwise = Illegal x


isIllegal :: CharStatus -> Bool
isIllegal (Illegal _) = True
isIllegal _ = False


autoCompleteScore :: CharStatus -> Integer
autoCompleteScore (Missing s) = foldl (\acc x -> acc * 5 + x) 0 numericScores
    where numericScores = map (autoCompleteScoreTable !) s
autoCompleteScore _ = error "Illegal call!"


main = do
    lines <- readData "input.txt"
    let lineStats = map evalString lines
    -- Part 1
    print $ sum $ map (\(Illegal x) -> errorScoreTable ! x) (filter isIllegal lineStats)
    -- Part 2
    let filteredLines = filter (not . isIllegal) lineStats
    let lineScores = map autoCompleteScore filteredLines
    print $ List.sort lineScores !! (length lineScores `div` 2)
