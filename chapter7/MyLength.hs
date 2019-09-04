module MyLength where
myLength :: [a] -> Int
-- myLength = foldr (\_ n -> 1 + n) 0
myLength = foldl (\n _ -> 1 + n) 0
