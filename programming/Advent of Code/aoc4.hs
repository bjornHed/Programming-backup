import Data.List.Split
import Data.Hashable

readInput :: FilePath -> IO Int
readInput p = do str <- readFile p
                 return $ foldr (+) 0 $ map checkRoom $ splitOn "\n" str

checkRoom :: String -> Int
checkRoom str = 
      if isOrdered (isCorrect checker abl []) checker 50000000 'a'
      then read val
      else 0
    where newstr    = splitOn "-" str
          checker   = take 5 $ last $ splitOn "[" $ last newstr
          val       = head $ splitOn "[" $ last newstr
          abl       = take ((length newstr)-1) newstr
          
isCorrect :: String -> [String] -> [Int] -> [Int]
isCorrect [] lst curr     = curr
isCorrect (x:xs) lst curr =
    isCorrect xs lst 
        (curr ++ [(foldr (+) 0 $ map length $ map (filter (==x)) lst)] )
    
{-isOrdered :: [Int] -> Int -> Bool
isOrdered [] n      = True
isOrdered (x:xs) n  = 
    if x == n
    if x > n
    then False
    else isOrdered xs x -}
    
isOrdered :: [Int] -> String -> Int -> Char -> Bool
isOrdered [] s n c = True
isOrdered (x:xs)(s:ss) n c =
    if x == n
    then if c > s
         then False
         else isOrdered xs ss x s
    else if x > n
         then False
         else isOrdered xs ss x s