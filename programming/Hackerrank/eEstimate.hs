eEstimate :: Int -> Double -> Float -> Float
eEstimate n x current
  | n == 0 = 1 + current
  | otherwise =
    eEstimate (n-1) x
      (current + ((x^n))/(fromIntegral (product([1..n]))))
