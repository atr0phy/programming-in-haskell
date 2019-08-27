module LuhnDouble where

luhnDouble :: Int -> Int
luhnDouble x
   | n < 10 = n
   | otherwise = n - 9
   where n = x * 2
