import qualified Data.Map as M
import qualified Data.Set as S


charPriorities :: M.Map Char Int
charPriorities =
    let lower_case = M.fromList $ zip ['a'..'z'] [1..26]
        upper_case = M.fromList $ zip ['A'..'Z'] [27..52]
    in M.union lower_case upper_case


pt1 :: String -> Int
pt1 knapsack = S.foldl (+) 0 item_priorities
    where
        num_items = length knapsack `div` 2
        left = take num_items knapsack
        right = drop num_items knapsack
        shared_items = S.intersection (S.fromList left) (S.fromList right)
        item_priorities = S.map (charPriorities M.!) shared_items


-- Part 2
chunkList :: Int -> [a] -> [[a]]
chunkList n [] = []
chunkList n xs = take n xs : chunkList n (drop n xs)


findCommonItemInGroup :: [[Char]] -> S.Set Char
findCommonItemInGroup = foldl1 S.intersection . map S.fromList


pt2 :: [[Char]] -> Int
pt2 group = sum $ S.map (charPriorities M.!) shared_items
    where
        shared_items = findCommonItemInGroup group


main :: IO ()
main = do
    contents <- readFile "input.txt"
    let input = lines contents
    --putStrLn $ show $ sum $ map pt1 input
    putStrLn $ show $ sum $ map pt2 (chunkList 3 input)
