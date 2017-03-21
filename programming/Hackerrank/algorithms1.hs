import Control.Applicative
import Control.Monad
import System.IO


main :: IO ()
main = do
    n_temp <- getLine
    let n_t = words n_temp
    let n = read $ n_t!!0 :: Int
    let k = read $ n_t!!1 :: Int
    let q = read $ n_t!!2 :: Int
    a_temp <- getLine
    let a = map read $ words a_temp :: [Int]
    let rot = rotate a k
    forM_ [1..q] $ \a0  -> do
        m_temp <- getLine
        let m = read m_temp :: Int
        putStr $ rot!!m


getMultipleLines :: Int -> IO [String]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do
        x <- getLine
        xs <- getMultipleLines (n-1)
        let ret = (x:xs)
        return ret

rotate :: [Int] -> Int -> [Int]
rotate lst 0 = lst
rotate lst n =
    let l = length lst in
      rotate ((last lst):(take (l-1) lst)) (n-1)
