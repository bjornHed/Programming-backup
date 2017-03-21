import Data.List

main = do
  n_temp <- getLine
  let n_t = words n_temp
  let n = map (\x -> read x :: Int) n_t
  let lst = sums n 5
  putStr $ show $ minimum lst
  putStr " "
  putStr $ show $ maximum lst

sums :: [Int] -> Int -> [Int]
sums lst 0 = []
sums lst n = [(sum ((take (n-1) lst) ++ (drop n lst)))] ++ sums lst (n-1)

mean :: [Int] -> Float
mean lst = (fromIntegral (sum lst)) / (fromIntegral (length lst))

median :: [Int] -> Float
median lst =
  let sorted = sort lst in
    if odd (length sorted)
    then fromIntegral $ sorted!!(div (length sorted) 2)
    else (fromIntegral ((sorted!!(div (length sorted) 2))
          + (sorted!!((div (length sorted) 2)-1)))) / 2

mode :: [Int] -> Int
mode lst =
  let sorted = sort lst in
    fst $ foldr (\(a,b) -> \(c,d) -> if d > b then (c,d) else (a,b)) (0,0)
      $ nub $ map (\x -> (x, getOccur x sorted)) sorted
        where getOccur _ []     = 0
              getOccur e (x:xs) =
                 if e == x then 1 + (getOccur e xs) else getOccur e xs
