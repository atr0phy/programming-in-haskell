module Chapter7.MyReverse where

myReverse :: [a] -> [a]
-- myReverse = foldr (\x xs -> xs ++ [x]) []
myReverse = foldl (\xs x -> x : xs) []
