import Data.List.Split
import Data.Maybe
import Data.List

{-checkString :: String -> Int
checkString []     = 0
checkString (x:xs)
    | length xs < 3 = 0
    | otherwise = if x == (xs !! 1) && (xs !! 0) == (xs !! 1) && x /= (xs !! 0)
                  then 1
                  else checkString xs
    
brackString :: String -> [String] -> [String]
brackString [] strs = strs
brackString str strs = 
    brackString (drop (j+1) str) (strs ++ [(take (j-i) (drop (i+1) str))])
  where i = case elemIndex '[' str of
                Nothing -> 0
                Just b  -> b 
        j = case elemIndex ']' str of
                Nothing -> 0
                Just b  -> b 
          
checkBracks :: String -> Bool
checkBracks strs = i == 0
   where i = foldr (+) 0 $ map checkString (brackString strs [])
   
checkAll :: [String] -> Int
checkAll strs = foldr (+) 0 $ map checkLine strs
    
checkLine :: String -> Int
checkLine str =
    if (checkBracks str)
    then if (foldr (+) 0 (map checkString (( splitOneOf "[.]" str )))) > 0
         then 1
         else 0
    else 0-}
getInput :: FilePath -> IO [String]
getInput p = do str <- readFile p
                return $ splitOn "\n" str

readInput :: FilePath -> IO Int
readInput p = do str <- readFile p
                 return $ checkAll $ toRows str
                 
toRows :: String -> [String]
toRows s = splitOn "\n" s

checkAll :: [String] -> Int
checkAll strs = foldr (+) 0 $ map checkLine strs

checkLine :: String -> Int
checkLine (x:xs) 
    | length xs < 2 = 0
    | otherwise =
        if x == (xs !! 1) && (x /= (xs !! 0))
        then if isInfixOf ([(xs !! 0)] ++ [x] ++ [(xs !! 0)]) (drop 2 xs)
             then 1
             else checkLine xs
        else checkLine xs
    

