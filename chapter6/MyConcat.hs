module MyConcat where
myConcat :: [[a]] -> [a]
myConcat []         = []
myConcat (xs : xss) = xs ++ myConcat xss
