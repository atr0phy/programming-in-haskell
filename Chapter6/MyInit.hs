module Chapter6.MyInit where

myInit :: [a] -> [a]
myInit [_     ] = []
myInit (x : xs) = x : myInit xs
