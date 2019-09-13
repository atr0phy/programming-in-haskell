module Chapter12.Tree where

import           Chapter12.State

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

{-
instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g (Leaf x  ) = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)
-}

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _  ) n = (Leaf n, n + 1)
rlabel (Node l r) n = (Node l' r', n'')
 where
  (l', n' ) = rlabel l n
  (r', n'') = rlabel r n'

fresh :: ST Int
fresh = S (\n -> (n, n + 1))

-- applicative style
alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _  ) = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r

-- monad style
mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _) = do
  n <- fresh
  return (Leaf n)

mlabel (Node l r) = do
  l' <- mlabel l
  r' <- mlabel r
  return (Node l' r')
