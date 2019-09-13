module Chapter12.Maybe where

{-
  instance Applicative Maybe where
    -- pure :: a -> Maybe a
    pure = Just

    -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    Nothing <*> _ = Nothing
    (Just g) <*> mx = fmap g mx
  
  instance Applicative Maybe where
    -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    Nothing >>= _ = Nothing
    (Just x) >>= f = f x
-}
