module Isort where

import           Insert

isort :: Ord a => [a] -> [a]
isort []       = []
isort (x : xs) = insert x (isort xs)
