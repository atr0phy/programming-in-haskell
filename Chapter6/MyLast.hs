module Chapter6.MyLast where

myLast :: [a] -> a
myLast [x     ] = x
myLast (_ : xs) = myLast xs
