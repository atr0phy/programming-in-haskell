module Chapter7.Sumsqreven where

sumsqreven :: [Int] -> Int
sumsqreven ns = sum (map (^ 2) (filter even ns))
