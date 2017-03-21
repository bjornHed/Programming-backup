import Data.List.Split
import Data.List

readInput :: FilePath -> IO String
readInput p = do str <- readFile p
                 return$ getWord (sortCols (putInCols (toRows str) ["","","","","","","",""])) ""
                 
toRows :: String -> [String]
toRows s = splitOn "\n" s

putInCols :: [String] -> [String] -> [String]
putInCols [] lst      = lst
putInCols (x:xs) lst  = 
    putInCols xs [((lst !! n) ++ [(x !! n)]) | n <- [0..7] ]
    
sortCols :: [String] -> [String]
sortCols lst = map sort lst

-- | Takes sorted columns
getWord :: [String] -> String -> String
getWord [] s     = s
getWord (x:xs) s = let Just i = elemIndex (minimum $ map length (group x)) (map length (group x)) in
                    getWord xs (s++[(head ((group x) !! i))])