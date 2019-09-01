module Msort where
import           Merge
import           Halve

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort xs') (msort ys')
 where
  half = halve xs
  xs'  = fst half
  ys'  = snd half

