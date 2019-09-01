module MyDrop where
myDrop :: Int -> [a] -> [a]
myDrop 0 xs       = xs
myDrop _ []       = []
myDrop n (_ : xs) = myDrop (n - 1) xs
