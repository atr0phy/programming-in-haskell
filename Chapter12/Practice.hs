module Chapter12.Practice where

-- 1
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g Leaf         = Leaf
  fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

-- 2
{-
  instance Functor ((->) a) where
    -- fmap :: (b -> c) -> (a -> b) -> (a -> c)
    fmap = (.)
-}

-- 3
{-
  instance Applicative ((->) a) where
    -- pure :: b -> (a -> b)
    pure = const

  -- (<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c)
    g <*> h = \x -> g x (h x)
-}

-- 4
