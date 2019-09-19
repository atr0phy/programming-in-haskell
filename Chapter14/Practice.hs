module Chapter14.Practice where

import           Data.Foldable

-- 1
{-
  instance (Monoid a, Monoid b) => Monoid (a, b) where
    -- mempty :: (a, b)
    mempty = (mempty, mempty)

    -- mappend :: (a, b) -> (a, b) -> (a, b)
    (x1, y1) `mappend` (x2, y2) = (x1 `mappend` x2, y1 `mappend` y 2)
-}

-- 2
{-
  instance Monoid b => Monoid (a -> b) where
    -- mempty :: a -> b
    mempty _ = mempty

    -- mappend :: (a -> b) -> (a -> b) -> (a -> b)
    f `mappend` g = \x -> f x `mappend` g x
-}

-- 3

newtype Mmaybe a = M (Maybe a)

instance Functor Mmaybe where
  -- fmap :: (a -> b) -> a -> b
  fmap _ (M Nothing ) = M Nothing
  fmap g (M (Just x)) = M (Just (g x))

instance Foldable Mmaybe where
  -- fold :: Mmaybe a -> a
  fold (M Nothing ) = mempty
  fold (M (Just x)) = x
-- foldMap :: (a -> b) -> Maybe a -> b
  foldMap f (M Nothing ) = mempty
  foldMap f (M (Just x)) = f x
-- foldr :: (a -> b -> b) -> b -> Mmaybe a -> b
  foldr _ v (M Nothing ) = v
  foldr f v (M (Just x)) = f x v
-- foldl :: (a -> b -> a) -> a -> Mmaybe b -> a
  foldl _ v (M Nothing ) = v
  foldl f v (M (Just x)) = f v x

instance Traversable Mmaybe where
  traverse g = sequenceA . fmap g

-- 4
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> a -> b
  fmap _ Leaf         = Leaf
  fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

instance Foldable Tree where
  -- fold :: Monoid a => Tree a -> a
  fold Leaf         = mempty
  fold (Node l x r) = fold l `mappend` x `mappend` fold r

-- foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap _ Leaf         = mempty
  foldMap f (Node l x r) = foldMap f l `mappend` f x `mappend` foldMap f r

-- foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ v Leaf         = v
  foldr f v (Node l x r) = foldr f (foldr f (f x v) r) l

-- foldl :: (a -> b -> a) -> a -> Tree b -> a
  foldl _ v Leaf         = v
  foldl f v (Node l x r) = foldl f (foldl f (f v x) l) r

instance Traversable Tree where
  traverse g = sequenceA . fmap g

-- 5
filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF g = foldMap (\a -> if g a then pure a else mempty)
