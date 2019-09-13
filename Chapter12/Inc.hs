module Chapter12.Inc where

inc :: Functor f => f Int -> f Int
inc = fmap (+ 1)
