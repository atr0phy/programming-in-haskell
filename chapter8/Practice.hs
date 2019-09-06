module Practice where

import           Nat

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
