module Chapter5.Count where

count :: Char -> String -> Int
count x xs = length [ x' | x' <- xs, x == x' ]
