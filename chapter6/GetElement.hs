module GetElement where
(!!) :: [a] -> Int -> a
(x : xs) !! 0 = x
(_ : xs) !! n = xs GetElement.!! (n - 1)
