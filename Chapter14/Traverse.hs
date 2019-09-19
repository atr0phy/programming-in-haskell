module Chapter14.Traverse where

myTraverse :: (a -> Maybe b) -> [a] -> Maybe [b]
myTraverse _ []       = pure []
myTraverse g (x : xs) = pure (:) <*> g x <*> myTraverse g xs

dec :: Int -> Maybe Int
dec n = if n > 0 then Just (n - 1) else Nothing
