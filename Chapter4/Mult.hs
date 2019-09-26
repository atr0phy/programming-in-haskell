module Chapter4.Mult where

mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> (x * y * z)))
