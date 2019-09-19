module Chapter14.Concat where

import           Data.Foldable

concat :: Foldable t => t [a] -> [a]
concat = fold
