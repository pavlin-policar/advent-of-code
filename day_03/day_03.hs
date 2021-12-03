import qualified Data.List as List
import qualified Data.Map as Map
import Data.Char (digitToInt)
import Data.Bits (shiftL)


countValues :: Ord a => [a] -> Map.Map a Int
countValues xs = Map.fromListWith (+) (map (\v -> (v, 1)) xs)


mostCommon :: Map.Map Char Int -> Char
mostCommon m = fst $ Map.foldlWithKey (\(k0, v0) k v -> if v0 > v then (k0, v0) else (k, v)) ('a', -1) m


leastCommon :: Map.Map Char Int -> Char
leastCommon m = fst $ Map.foldlWithKey (\(k0, v0) k v -> if v0 < v then (k0, v0) else (k, v)) ('a', 99999999) m


enumerate = zip [0..]


fromBinary :: String -> Int
fromBinary s = foldl (\acc (off, v) -> acc + (v `shiftL` off)) 0 (enumerate (reverse binaryList))
    where binaryList = map digitToInt s


main = do
    contents <- readFile "input.txt"
    let ls = lines contents
    let charCounts = map countValues (List.transpose ls)
    let gamma = fromBinary $ map mostCommon charCounts
    let epsilon = fromBinary $ map leastCommon charCounts
    putStrLn $ show $ gamma * epsilon
