module MyTree where

data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs' :: Eq a => a -> Tree a -> Bool
occurs' x (Leaf y    ) = x == y
occurs' x (Node l y r) = x == y || occurs' x l || occurs' x r

-- flatten Tree -> Get Sorted List -> It's Search Tree
flatten :: Tree a -> [a]
flatten (Leaf x    ) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

occurs'' :: Ord a => a -> Tree a -> Bool
occurs'' x (Leaf y) = x == y
occurs'' x (Node l y r) | x == y    = True
                        | x < y     = occurs'' x l
                        | otherwise = occurs'' x r

-- 2
occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y    ) = x == y
occurs x (Node l y r) = case compare x y of
  LT -> occurs x l
  EQ -> True
  GT -> occurs x r


-- Only leaf has data
-- data Tree a = Leaf a | Node (Tree a) (Tree a)
-- Only node has data
-- data Tree a = Leaf | Node (Tree a) a (Tree a)
-- Leaf and node has different type data
-- data Tree a b = Leaf a | Node (Tree a b) b (Tree a b)
-- Tree with a list of subtrees
-- data Tree a = Node a [Tree a]
