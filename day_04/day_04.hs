import qualified Data.Text as T
import qualified Data.List as L

type Board a = [[a]]
type MaskedBoard a = [[(a, Bool)]]
type Mask = [[Bool]]


chunkList n [] = []
chunkList n xs = (take n xs):(chunkList n (drop n xs))


addMask :: Board a -> MaskedBoard a
addMask xs = map (map (\x -> (x, False))) xs


extractMask :: MaskedBoard a -> Mask
extractMask board = map (map snd) board


updateBoard :: Eq a => MaskedBoard a -> a -> MaskedBoard a
updateBoard board val = map (updateBoardRow val) board
    where updateBoardRow val boardRow = map (\(x, prev) -> (x, prev || x == val)) boardRow


isWinningBoard :: MaskedBoard a -> Bool
isWinningBoard board = any (== True) (cols ++ rows)
    where mask = extractMask board
          rows = (map (all (== True)) mask)
          cols = (map (all (== True)) (L.transpose mask))


takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive c [] = []
takeWhileInclusive c (x:xs) = x : if c x then takeWhileInclusive c xs else []


-- Calculate the sum of the unmarked numbers on the board
sumScoreWinningBoard :: MaskedBoard String -> Int
sumScoreWinningBoard board = foldl f 0 (concat board)
    where f acc (val, flag) = if not flag then acc + (read val :: Int) else acc


-- Utility functions to select from tuple
tup3fst (x, _, _) = x
tup3snd (_, x, _) = x
tup3thd (_, _, x) = x


firstToWin = do
    contents <- readFile "input.txt"
    let parts = map words $ lines contents
    let numbers = map T.unpack $ T.splitOn (T.pack ",") $ T.pack $ (head parts) !! 0
    let boards = map addMask $ chunkList 5 $ filter (not . null) (tail parts)
    -- Run the game using the input
    let gameplay = map (\b -> scanl updateBoard b numbers) boards
    let gameplayWithNumbers = map (zip ("0" : numbers)) gameplay
    -- For each move, check if any of the boards are winning boards
    let isWinning = map (map (\(v, b) -> (isWinningBoard b, v, b))) (L.transpose gameplayWithNumbers)
    -- Keep playing the game until at least one of the boards is a winning board
    let finalMove = last $ takeWhileInclusive (all (\(w, v, b) -> w == False)) isWinning
    -- Extract the winning board from the final move
    let winningBoard = (!! 0) $ filter (\(w, v, b) -> w) finalMove
    let finalNumber = read (tup3snd winningBoard) :: Int
    let winningBoardSum = sumScoreWinningBoard (tup3thd winningBoard)
    putStrLn $ show finalNumber
    putStrLn $ show winningBoardSum
    putStrLn $ show $ finalNumber * winningBoardSum


lastToWin = do
    contents <- readFile "input.txt"
    let parts = map words $ lines contents
    let numbers = map T.unpack $ T.splitOn (T.pack ",") $ T.pack $ (head parts) !! 0
    let boards = map addMask $ chunkList 5 $ filter (not . null) (tail parts)
    -- Run the game using the input
    let gameplay = map (\b -> scanl updateBoard b numbers) boards
    let gameplayWithNumbers = map (zip (numbers)) gameplay
    -- For each move, check if any of the boards are winning boards
    let isWinning = map (map (\(v, b) -> (isWinningBoard b, v, b))) (L.transpose gameplayWithNumbers)
    -- Keep playing the game until at least one of the boards is a winning board
    let finalMove = last $ takeWhile (any (\(w, v, b) -> w == False)) isWinning
    -- Extract the winning board from the final move
    let losingBoard = (!! 0) $ filter (\(w, v, b) -> not w) finalMove
    let finalNumber = read (tup3snd losingBoard) :: Int
    let winningBoardSum = sumScoreWinningBoard (updateBoard (tup3thd losingBoard) (tup3snd losingBoard))
    putStrLn $ show finalNumber
    putStrLn $ show winningBoardSum
    putStrLn $ show $ finalNumber * winningBoardSum


main = lastToWin
