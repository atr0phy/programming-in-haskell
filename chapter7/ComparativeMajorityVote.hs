module ComparativeMajorityVote where

import           Data.List

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

-- remove duplicates
rmdups :: Eq a => [a] -> [a]
rmdups []       = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

result :: Ord a => [a] -> [(Int, a)]
result vs = sort [ (count v vs, v) | v <- rmdups vs ]

winner :: Ord a => [a] -> a
winner = snd . last . result
