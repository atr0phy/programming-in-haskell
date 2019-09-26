module Chapter7.MySum where

mySum :: Num a => [a] -> a
mySum = foldl (+) 0
