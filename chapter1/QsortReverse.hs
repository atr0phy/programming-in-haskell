qsortReverse [] = []
qsortReverse (x:xs) = qsortReverse larger ++ [x] ++ qsortReverse smaller
               where
                  smaller = [a | a <- xs, a <= x]
                  larger = [b | b <- xs, b > x]
