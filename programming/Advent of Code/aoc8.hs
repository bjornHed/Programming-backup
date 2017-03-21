import Data.List
import Data.List.Split

data Disp = Disp { rows :: [[Int]] }
    deriving ( Show )

startDisp :: Disp
startDisp = Disp (replicate 6 (replicate 50 0))

readInput :: FilePath -> IO Int
readInput p = do s <- readFile p
                 let (Disp dir) = runProgram (splitOn "\n" s) startDisp in
                    return $ nmbrOfBits dir 0
           
                 
nmbrOfBits :: [[Int]] -> Int -> Int
nmbrOfBits [] n = n
nmbrOfBits (x:xs) n = nmbrOfBits xs (n + (sum x))
  
runProgram :: [String] -> Disp -> Disp
runProgram [] disp     = disp
runProgram (x:xs) disp = runProgram xs (interpret x disp)
  
interpret :: String -> Disp -> Disp
interpret str (Disp dis) = 
    if isInfixOf "rect" str
    then Disp ((map upd (take x dis)) ++ (drop x dis))
    else if isInfixOf "row" str
         then Disp $ dis !!= (colx,(drop (50-coly) (dis !! colx)) ++ (take (50-coly) (dis !! colx)))
         else Disp $ transpose $ transpdisp !!= (rowy,(drop (6-rowx) (transpdisp !! rowy)) ++ (take (6-rowx) (transpdisp !! rowy)))
  where y = if length str > 8
            then read $ [(drop 5 str) !! 0] ++ [(drop 6 str) !! 0]
            else read $ [(drop 5 str) !! 0]
        x = read $ [last str]
        transpdisp = transpose dis
        upd v = (replicate y 1) ++ (drop y v)
        colx = (read [str !! 13]) 
        coly = if length str > 18
               then (read ([str !! 18]++[str !! 19]))
               else (read [str !! 18]) 
        rowy = if length str > 21
               then (read ([str !! 16]++[str !! 17]))
               else (read [str !! 16]) 
        rowx = if length str > 21
               then (read [str !! 22]) 
               else (read [str !! 21]) 
        
-- | Replaces an element at a given index in the list with a new value
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) [] _ = []
(!!=) xs (index,val)
        | index >= length xs = (take ((length before)-1) before) ++ [val]
        | otherwise = before ++ [val] ++ tail after
    where (before, after) = splitAt index xs