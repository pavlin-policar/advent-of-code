import qualified Data.Set as Set


solve :: String -> Int
solve s = solve' 0 s
    where
        solve' :: Int -> String -> Int
        solve' i s = if Set.size (Set.fromList startSeq) == 4 then i + 4 else solve' (i + 1) (tail s)
            where
                startSeq = take 4 s


main = do
    input <- readFile "input.txt"
    print $ solve input
