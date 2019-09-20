module Chapter15.Primes where

primes :: [Int]
primes = sieve [2 ..]

sieve :: [Int] -> [Int]
sieve (p : xs) = p : [ x | x <- xs, x `mod` p /= 0 ]
