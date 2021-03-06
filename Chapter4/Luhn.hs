module Chapter4.Luhn where

import           Chapter4.LuhnDouble

luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z = (luhnDouble w + x + luhnDouble y + z) `mod` 10 == 0
