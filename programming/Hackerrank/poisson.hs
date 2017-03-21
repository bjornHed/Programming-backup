-- Enter your code here. Read input from STDIN. Print output to STDOUT
main = do
    x <- getLine
    y <- getLine
    let e = 2.71828 :: Float
    let m = 2.5
    let n = 5 :: Int
    let res = ((e**(-m))*(m^n))/(product [1..(fromIntegral n)])
    putStr $ show $ (fromInteger $ round $ res * (10^3)) / (10.0^^3)
