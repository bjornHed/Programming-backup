-- Parafunk, lecture 1

{-The number of transistors per chip increases by
  a factor of two every year - Moores Law

* Branch prediction
* Value speculation

Race condition leads to incorrect, non-deterministic behaviour

Locking (semaphors osv) are error prone
Locking leads to deadlock and other concurrency errors
Locking is costly

"The speed-up of a program on a paralell computer is limited
by the time spent in the sequential part" - Amdahl's Law

* if 5% of the time i sequential then the maximum speedup is 20x

-}
import Control.Parallel
--import Criterion.Main
{- ghc -O2
    -threaded
    -rtsopts
    -eventlog-}

-- | Returns number of calls made.
nfib :: Integer -> Integer
nfib n | n<2 = 1
nfib n = nfib (n-1) + nfib (n-2) + 1

main2 = print(nfib 45)
main = print(rfib 45)

nfib2 :: Integer -> Integer
nfib2 n | n<2 = 1
nfib2 n = par nf (nfib (n-2) + nf + 1)
  where nf = nfib2 (n-1)

rfib :: Integer -> Integer
rfib n | n < 2 = 1
rfib n = par nf1 (pseq nf2 (nf1 + nf2 + 1))
  where nf1 = rfib (n-1)
        nf2 = rfib (n-2)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [y | y <- xs, y < x]
            ++ [x]
            ++ qsort [y | y <- xs, y >= x]
