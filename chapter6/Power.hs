module Power where
(^) :: Int -> Int -> Int
_ ^ 0 = 1
m ^ n = m * (m Power.^ (n - 1))
