import qualified Data.Text as T


readData :: FilePath -> IO [[[String]]]
readData fname = do
    contents <- readFile fname
    let ls = map (map T.strip . T.splitOn (T.pack "|") . T.pack) $ lines contents
    return $ map (map (map T.unpack . T.splitOn (T.pack " "))) ls


main = do
    contents <- readData "input.txt"
    let lengths = map (map length . (!! 1)) contents
    let identifiables = map (sum . map (fromEnum . (flip elem) [2, 3, 4, 7])) lengths
    putStrLn $ show $ sum identifiables
