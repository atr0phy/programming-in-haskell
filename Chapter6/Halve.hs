module Chapter6.Halve where

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs
