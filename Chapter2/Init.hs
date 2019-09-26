module Chapter2.Init where

-- myInit xs = take ((length xs) - 1) xs
myInit xs = reverse (drop 1 (reverse xs))
