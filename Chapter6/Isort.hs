module Chapter6.Isort where

import           Chapter6.Insert

isort :: Ord a => [a] -> [a]
isort []       = []
isort (x : xs) = insert x (isort xs)
