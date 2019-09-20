module Chapter15.SumWith where

sumwith :: Int -> [Int] -> Int
-- lazy evaluation
-- sumwith v []       = v
-- sumwith v (x : xs) = sumwith (v + x) xs
-- eager evaluation
sumwith v []       = v
sumwith v (x : xs) = (sumwith $! (v + x)) xs
-- or
-- import Data.Foldable
-- sumwith = foldl' (+)
