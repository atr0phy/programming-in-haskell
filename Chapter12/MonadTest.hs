module Chapter12.MonadTest where

import           Data.Char
import           Control.Monad           hiding ( mapM
                                                , filterM
                                                , join
                                                )
import           Prelude                 hiding ( mapM )

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f []       = return []
mapM f (x : xs) = do
  y  <- f x
  ys <- mapM f xs
  return (y : ys)

conv :: Char -> Maybe Int
conv c | isDigit c = Just (digitToInt c)
       | otherwise = Nothing

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM p []       = return []
filterM p (x : xs) = do
  b  <- p x
  ys <- filterM p xs
  return (if b then x : ys else ys)

join :: Monad m => m (m a) -> m a
join mmx = do
  mx <- mmx
  x  <- mx
  return x
