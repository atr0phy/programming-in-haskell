module Chapter6.MySum where

mySum :: Num a => [a] -> a
mySum []       = 0
mySum (x : xs) = x + mySum xs
