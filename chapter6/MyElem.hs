module MyElem where
myElem :: Eq a => a -> [a] -> Bool
myElem _ []       = False
myElem x (y : ys) = if x == y then True else myElem x ys
