halve :: [a] -> ([a], [a])
halve xs = (take middle xs, drop middle xs)
           where
              middle = length xs `div` 2
