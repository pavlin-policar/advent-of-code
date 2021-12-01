import System.IO
import Control.Monad


findNumIncreasing xs = sum $ map fromEnum $ map (\t -> (fst t) < (snd t)) pairs
    where pairs = zip xs (tail xs)


{-
main =
    putStrLn $ show $ findNumIncreasing [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
-}


main = do
    contents <- readFile "input.txt"
    --ls_ints <- convertToIntList contents
    putStrLn $ show $ findNumIncreasing $ map (read::String -> Int) $ lines contents
