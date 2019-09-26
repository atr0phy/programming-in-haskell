module Chapter6.Fac where

fac :: Int -> Int
fac 0         = 1
fac n | n > 0 = n * fac (n - 1)
