module Chapter5.Practice where

import           Chapter5.Factors
import           Chapter5.Find

p1 :: Int
p1 = sum [ x ^ 2 | x <- [1 .. 100] ]

grid :: Int -> Int -> [(Int, Int)]
grid m n = [ (x, y) | x <- [0 .. m], y <- [0 .. n] ]

square :: Int -> [(Int, Int)]
square n = [ (x, y) | (x, y) <- grid n n, x /= y ]

myReplicate :: Int -> a -> [a]
myReplicate n x = [ x | _ <- [1 .. n] ]

pyths :: Int -> [(Int, Int, Int)]
pyths n =
  [ (x, y, z)
  | x <- [1 .. n]
  , y <- [1 .. n]
  , z <- [1 .. n]
  , x ^ 2 + y ^ 2 == z ^ 2
  ]

perfects :: Int -> [Int]
perfects n = [ x | x <- [1 .. n], sum (init (factors x)) == x ]

p7 :: [(Int, Int)]
p7 = concat [ [ (x, y) | y <- [4, 5, 6] ] | x <- [1, 2, 3] ]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0 ..])

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs xss = sum [ x * y | (x, y) <- zip xs xss ]

