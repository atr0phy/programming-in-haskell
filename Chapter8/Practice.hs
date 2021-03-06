module Chapter8.Practice where

import           Chapter8.Nat

-- 1
nat1 = Succ (Succ (Succ (Succ (Succ Zero))))
nat2 = Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))

mult :: Nat -> Nat -> Nat
mult Zero        _           = Zero
mult _           Zero        = Zero
mult m           (Succ Zero) = m
mult (Succ Zero) n           = n
mult m           (Succ n)    = add m (mult m n)

-- 3
data Tree a = Leaf a | Node (Tree a) (Tree a)
t1 = Leaf 1
t2 = Node (Leaf 1) (Leaf 3)
t3 = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))
t4 = Node (Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))) (Leaf 1)
t5 = Node (Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4)))
          (Node (Node (Leaf 5) (Leaf 6)) (Node (Leaf 7) (Leaf 8)))
t6 = Node (Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4)))
          (Node (Node (Leaf 5) (Leaf 6)) (Leaf 7))

balanced :: Tree a -> Bool
balanced (Leaf _                ) = True
balanced (Node (Leaf _) (Leaf _)) = True
balanced (Node x        y       ) = abs ((leaves x) - (leaves y)) <= 1

leaves :: Tree a -> Int
leaves (Leaf _  ) = 1
leaves (Node x y) = leaves x + leaves y

-- 4
balance :: [a] -> Tree a
balance (x : []) = Leaf x
balance xs       = Node (balance $ fst halved) (balance $ snd halved)
  where halved = half xs

describe :: Show a => Tree a -> String
describe (Leaf a  ) = "(Leaf " ++ (show a) ++ ")"
describe (Node a b) = "(Node " ++ (describe a) ++ " " ++ (describe b) ++ ")"

half :: [a] -> ([a], [a])
half [] = ([], [])
half xs = splitAt ((length xs) `div` 2) xs

-- 5
data Expr = Val Int | Add Expr Expr

expr1 = Add (Add (Val 1) (Val 2)) (Add (Val 3) (Val 4))
expr2 = Add (Add (Val 4) (Val 4)) (Add (Val 5) (Val 5))
expr3 = Add (Add (Val 1) (Val 2)) (Val 3)

-- folde id (+) expr1

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val n  ) = f n
folde f g (Add d e) = g (folde f g d) (folde f g e)

-- 6
eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+)

-- 7
-- instance Eq a => Eq (Maybe a) where
--   Nothing == Nothing = True
--   Just a  == Just b  = a == b

-- instance Eq a => Eq [a] where
--   []       == []       = True
--   (x : xs) == (y : ys) = x == y && xs == ys
--   xs       == ys       = False
