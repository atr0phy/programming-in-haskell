module Chapter12.Expr where

data Expr = Val Int | Div Expr Expr

-- eval :: Expr -> Int
-- eval (Val n  ) = n
-- eval (Div x y) = eval x `div` eval y

-- eval :: Expr -> Maybe Int
-- eval (Val n  ) = Just n
-- eval (Div x y) = case eval x of
--   Nothing -> Nothing
--   Just n  -> case eval y of
--     Nothing -> Nothing
--     Just m  -> safediv n m

-- eval :: Expr -> Maybe Int
-- eval (Val n  ) = Just n
-- eval (Div x y) = eval x >>= \n -> eval y >>= \m -> safediv n m

eval :: Expr -> Maybe Int
eval (Val n  ) = Just n
eval (Div x y) = do
  n <- eval x
  m <- eval y
  safediv n m

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)
