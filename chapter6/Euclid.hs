module Euclid where

euclid :: Int -> Int -> Int
euclid n m | n == m    = n
           | n > m     = euclid m (n - m)
           | n < m     = euclid n (m - n)
           | otherwise = 1
