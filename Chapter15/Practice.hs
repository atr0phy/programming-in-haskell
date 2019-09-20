module Chapter15.Practice where

-- 4
fibs :: [Integer]
fibs = 0 : 1 : [ x + y | (x, y) <- zip fibs (tail fibs) ]

-- 6
approx = 1.0
delta = 0.00001
sqroot :: Double -> Double
sqroot n = head . filter (\x -> abs (x * x - n) < delta) $ approxs
 where
  next a = (a + n / a) / 2
  approxs = iterate next approx
