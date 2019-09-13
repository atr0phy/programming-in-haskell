module Chapter12.List where

{-
  instance Functor [] where
    -- fmap :: (a -> b) -> f a -> f b
    fmap g [] = []
    fmap g (x:xs) = fmap g xs ++ [g x]

  -- actual code
  instance Functor [] where
    -- fmap :: (a -> b) -> [a] -> [b]
    fmap = map

  instance Applicative [] where
    -- pure :: a -> [a]
    pure x = [x]

    -- (<*>) :: [a -> b] -> [a] -> [b]
    gs <*> xs = [g x | g <- gs, x <- xs]
  
  instance Monad [] where
    -- (>>=) :: [a] -> (a -> [b]) -> [b]
    xs >>= f = [y | x <- xs, y  <- f x]
    
-}

{-
  fmap id /= id 

  > fmap id [1,2]
  [2,1]
  > id [1,2]
  [1,2]

  fmap (g . h) /= fmap g . fmap h
  > fmap (not . even) [1,2]
  [False True]
  > (fmap not . fmap even) [1,2]
  [True False]
-}
