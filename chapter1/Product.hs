myProduct [] = 0
myProduct [x] = x
myProduct (x:xs) = x * myProduct xs
