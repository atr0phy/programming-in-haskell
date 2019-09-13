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

newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
  -- fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap g (Z xs) = Z (fmap g xs)

instance Applicative ZipList where
  -- pure :: a -> ZipList a
  -- (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  pure x = Z (repeat x)
  (Z gs) <*> (Z xs) = Z [ g x | (g, x) <- zip gs xs ]

-- 6
{-
  instance Monad ((->) a) where
    f >>= k = \x -> k (f x) x
-}

-- 7
data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

instance Functor Expr where
  -- fmap :: Functor f => (a -> b) -> f a -> f b
  -- fmap :: Functor f => (a -> b) -> Expr a -> Expr b
  fmap g (Var a  ) = Var (g a)
  fmap _ (Val n  ) = Val n
  fmap g (Add x y) = Add (fmap g x) (fmap g y)

instance Applicative Expr where
  -- pure :: Applicative a => a -> Expr a
  -- <*> :: Expr (a -> b) -> Expr a -> Expr b
  pure = Var
  Var g <*> ex = fmap g ex

instance Monad Expr where
  -- (>>=) :: Monad f => f a -> (a -> f b) -> f b
  Var x   >>= g = g x
  Val x   >>= _ = Val x
  Add x y >>= g = Add (x >>= g) (y >>= g)

-- 8
type State = Int

-- type ST = State -> State
-- type ST a = State -> (a, State)
newtype ST a = S (State -> (a, State))

instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap g st = do
    s <- st
    return $ g s

instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (\s -> (x, s))

-- (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = do
    f <- stf
    x <- stx
    return $ f x

instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s -> let (x, s') = app st s in app (f x) s')

app :: ST a -> State -> (a, State)
app (S st) = st
