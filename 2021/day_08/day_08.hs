{-# LANGUAGE TupleSections #-}
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Set ((\\))
import Data.Map ((!))


readData :: FilePath -> IO [[[String]]]
readData fname = do
    contents <- readFile fname
    let ls = map (map T.strip . T.splitOn (T.pack "|") . T.pack) $ lines contents
    return $ map (map (map T.unpack . T.splitOn (T.pack " "))) ls


tuplify2 :: [b] -> (b, b)
tuplify2 [x, y] = (x, y)


data Number = Unknown | Number Int deriving (Show)


decodeByLength :: M.Map (Set.Set Char) Number -> M.Map (Set.Set Char) Number
decodeByLength mapping = M.fromList $ map f $ M.toList mapping
    where
        f (s, Unknown) = case length s of
            2 -> (s, Number 1)
            3 -> (s, Number 7)
            4 -> (s, Number 4)
            7 -> (s, Number 8)
            _ -> (s, Unknown)
        f x = x


decodeBySegments :: M.Map (Set.Set Char) Number -> M.Map (Set.Set Char) Number
decodeBySegments mapping = M.fromList $ map f (M.toList mapping)
    where
        -- Create a reverse map so we can enable string lookup by number
        reverseMapping = M.fromList $ map f (M.toList mapping)
            where
                f (x, Number y) = (y, x)
                f (x, Unknown) = (-1, x)
        -- Decode the numbers using set arithmetic
        f (s, Unknown) = case length s of
            5 -> (
                    if length (s \\ (reverseMapping ! 4)) == 3 then (s, Number 2)
                    else if length (s \\ (reverseMapping ! 7)) == 2 then (s, Number 3)
                    else (s, Number 5)
                )
            6 -> (
                    if length (s \\ (reverseMapping ! 4)) == 2 then (s, Number 9)
                    else if length (s \\ (reverseMapping ! 7)) == 3 then (s, Number 0)
                    else (s, Number 6)
                )
            _ -> (s, Unknown)
        f x = x


extractNumberMapping :: M.Map (Set.Set Char) Number -> M.Map (Set.Set Char) Int
extractNumberMapping mapping = M.fromList $ map extractNumbers (M.toList mapping)
    where
        extractNumbers (s, Number x) = (s, x)
        extractNumbers _ = error "Unkown encountered!"


decodeMapping :: [String] -> M.Map (Set.Set Char) Int
decodeMapping strings = (extractNumberMapping . decodeBySegments . decodeByLength) initialMapping
    where initialMapping = M.fromList $ map (\x -> (Set.fromList x, Unknown)) strings


decodePair (code, numbers) = read (concatMap show decodedNumbers) :: Int
    where
        mapping = decodeMapping code
        decodedNumbers = map ((mapping !) . Set.fromList) numbers


main = do
    contents <- readData "input.txt"
    print $ sum $ map (decodePair . tuplify2) contents
