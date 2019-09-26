module Chapter5.Positions where

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [ i | (x', i) <- zip xs [0 ..], x == x' ]
