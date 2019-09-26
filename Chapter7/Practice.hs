module Chapter7.Practice where

import           Chapter7.BinaryConverter       ( Bit )

-- 1
p1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
p1 f p xs = map f (filter p xs)

-- 2a
all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

-- 2b
any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

-- 2c
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x : xs) | p x       = x : takeWhile' p xs
                      | otherwise = []

-- 2d
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x : xs) | p x       = dropWhile' p xs
                      | otherwise = x : xs

-- 3
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> (f x) : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if (p x) then x : xs else xs) []

-- 4
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10 * x + y) 0

-- 5
{-
hoge = curry' sub
hoge 1 2
-1

unhoge = uncurry' sub'
unhoge (1,2)
-1
-}

sub :: Num a => (a, a) -> a
sub (x, y) = x - y

sub' :: Num a => a -> a -> a
sub' x y = x - y

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b

-- 6
unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

int2bin :: Int -> [Bit]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold (== []) (take 8) (drop 8)

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold null (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (\_ -> False) id f

-- 9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ []           = []
altMap f _ (x     : []) = f x : []
altMap f g (x : y : xs) = f x : g y : altMap f g xs


-- 10
luhnDouble :: Int -> Int
luhnDouble x | n < 10    = n
             | otherwise = n - 9
  where n = x * 2

luhn :: [Int] -> Bool
luhn ns = sum (altMap id luhnDouble (reverse ns)) `mod` 10 == 0
