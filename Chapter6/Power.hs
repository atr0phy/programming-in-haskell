module Chapter6.Power where

(^) :: Int -> Int -> Int
_ ^ 0 = 1
m ^ n = m * (m Chapter6.Power.^ (n - 1))
