module Chapter12.IO where

{-
  instance Functor IO where
    -- fmap :: (a -> b) -> IO a -> IO b
    fmap g mx = mx >>= (return . g)

  instance Applicative IO where
    -- pure :: a -> IO a
    pure = return

    -- (<*>) :: IO (a -> b) -> IO a -> IO b
    mg <*> mx = do (g <- mg; x <- mx; return (g x))

  instance Monad IO where
    -- return :: a -> IO a
    return x = ...

    -- (>>=) :: IO a -> (a -> IO) -> IO b
    mx >>= f ...
-}
