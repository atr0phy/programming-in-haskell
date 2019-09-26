module Chapter6.MyTake where

myTake :: Int -> [a] -> [a]
myTake 0 _        = []
myTake _ []       = []
myTake n (x : xs) = x : myTake (n - 1) xs
