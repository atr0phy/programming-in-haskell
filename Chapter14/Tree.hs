module Chapter14.Tree where

import           Data.Foldable

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> a -> b
  fmap g (Leaf x  ) = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)

instance Foldable Tree where
  -- fold :: Monoid a => Tree a -> a
  fold (Leaf x  ) = x
  fold (Node l r) = fold l `mappend` fold r

-- foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap f (Leaf x  ) = f x
  foldMap f (Node l r) = foldMap f l `mappend` foldMap f r

-- foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f v (Leaf x  ) = f x v
  foldr f v (Node l r) = foldr f (foldr f v r) l

-- foldl :: (a -> b -> a) -> a -> Tree b -> a
  foldl f v (Leaf x  ) = f v x
  foldl f v (Node l r) = foldl f (foldl f v l) r

instance Traversable Tree where
  -- traverse :: Aplicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse g (Leaf x  ) = pure Leaf <*> g x
  traverse g (Node l r) = pure Node <*> traverse g l <*> traverse g r
