import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map ((!))
import Data.Char (digitToInt)
import Data.Bits (shiftL)


-- Pt. 1
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


-- Pt. 2
findOxygen vals pos = if (length vals) > 1
        then let newVals = filter (\x -> (x !! pos) == filterVal) vals
             in findOxygen newVals (pos + 1)
        else vals
    where filterVal = criteria (countValues ((List.transpose vals) !! pos))
          criteria m = if (m ! '0') == (m ! '1')
                       then '1'
                       else if (m ! '0') < (m ! '1')
                       then '1'
                       else '0'


findCO2 vals pos = if (length vals) > 1
        then let newVals = filter (\x -> (x !! pos) == filterVal) vals
             in findCO2 newVals (pos + 1)
        else vals
    where filterVal = criteria (countValues ((List.transpose vals) !! pos))
          criteria m = if (m ! '0') == (m ! '1')
                       then '0'
                       else if (m ! '0') < (m ! '1')
                       then '0'
                       else '1'


main = do
    contents <- readFile "input.txt"
    let ls = lines contents
    -- Pt. 1
    let charCounts = map countValues (List.transpose ls)
    let gamma = fromBinary $ map mostCommon charCounts
    let epsilon = fromBinary $ map leastCommon charCounts
    putStrLn $ show $ gamma * epsilon
    -- Pt. 2
    let oxygenVal = fromBinary . head $ findOxygen ls 0
    let co2Val = fromBinary . head $ findCO2 ls 0
    putStrLn $ show $ oxygenVal * co2Val
