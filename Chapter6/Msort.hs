module Chapter6.Msort where
import           Chapter6.Merge
import           Chapter6.Halve

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort xs') (msort ys')
 where
  half = halve xs
  xs'  = fst half
  ys'  = snd half

