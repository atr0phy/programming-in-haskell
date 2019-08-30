module Find where
find :: Eq a => a -> [(a, b)] -> [b]
find k t = [ v | (k', v) <- t, k == k' ]
