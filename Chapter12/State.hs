{-# LANGUAGE TupleSections #-}

module Chapter12.State where

type State = Int

-- type ST = State -> State
-- type ST a = State -> (a, State)
newtype ST a = S (State -> (a, State))

instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap g st = S (\s -> let (x, s') = app st s in (g x, s'))

instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (x, )

-- (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = S
    (\s ->
      let (f, s' ) = app stf s
          (x, s'') = app stx s'
      in  (f x, s'')
    )

instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s -> let (x, s') = app st s in app (f x) s')

app :: ST a -> State -> (a, State)
app (S st) = st
