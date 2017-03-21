import Data.List.Split
 {-
readInput :: FilePath -> IO Int
readInput p = do str <- readFile p
                 return $ checkRows $ splitOn "\n" str
                
checkRows :: [String] -> Int
checkRows []     = 0
checkRows (x:xs) = (getSides x) + (checkRows xs)
    where getSides v = checkPossible $ splitOn " " v

checkPossible :: [String] -> Int
checkPossible list = 
    if (x + y > z) && (x + z > y) && (z + y > x)
    then 1
    else 0
  where lst = filter (/="") list
        x = read $ lst !! 0
        y = read $ lst !! 1
        z = read $ lst !! 2-}
        
----------------------------------------------------------

readInput :: FilePath -> IO Int
readInput p = do str <- readFile p
                 return $ checkTriangles (createCols (splitOn "\n" str) [[],[],[]]) 0
                 
createCols :: [String] -> [[Int]] -> [[Int]]
createCols [] current = current
createCols (list:xs) current = 
    createCols xs 
        [((current !! 0) ++ [x]),
        ((current !! 1) ++ [y]),((current !! 2) ++ [z])]
  where lol = splitOn " " list
        lst = filter (/="") lol
        x = read $ lst !! 0
        y = read $ lst !! 1
        z = read $ lst !! 2
                 
checkTriangles :: [[Int]] -> Int -> Int
checkTriangles lst curr = 
    if null (lst !! 0)
    then curr
    else checkTriangles 
            [(drop 3 (lst !! 0)),(drop 3 (lst !! 1)),(drop 3 (lst !! 2))] 
            (curr + checkTriangle (take 3 (lst !! 0))
            + checkTriangle (take 3 (lst !! 1))
            + checkTriangle (take 3 (lst !! 2)))
    
checkTriangle :: [Int] -> Int
checkTriangle lst =
    if (x + y > z) && (x + z > y) && (z + y > x)
    then 1
    else 0
  where x = lst !! 0
        y = lst !! 1
        z = lst !! 2
    
