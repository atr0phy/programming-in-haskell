module Safe where

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Mayve a
safehead [] = Nothing
safehead xs = Just (head xs)
