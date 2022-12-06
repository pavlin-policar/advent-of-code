import qualified Data.Set as Set


solve :: Int -> String -> Int
solve k s = solve' 0 s
    where
        solve' :: Int -> String -> Int
        solve' i s = if Set.size (Set.fromList startSeq) == k then i + k else solve' (i + 1) (tail s)
            where
                startSeq = take k s


pt1 = solve 4

pt2 = solve 14


main = do
    input <- readFile "input.txt"
    print $ pt2 input
