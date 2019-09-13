module Chapter12.Pairs where

pairs :: [a] -> [b] -> [(a, b)]
pairs xs ys = do
  x <- xs
  y <- ys
  return (x, y)

{-
  pairs :: [a] -> [b] -> [(a,b)]
  pairs xs ys = [(x,y) | x <- xs, y <-ys]
-}
