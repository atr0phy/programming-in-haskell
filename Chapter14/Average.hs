module Chapter14.Average where

average :: Foldable t => t Int -> Int
average ns = sum ns `div` length ns
