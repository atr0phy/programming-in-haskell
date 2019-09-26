module Chapter6.GetElement where

(!!) :: [a] -> Int -> a
(x : xs) !! 0 = x
(_ : xs) !! n = xs Chapter6.GetElement.!! (n - 1)
