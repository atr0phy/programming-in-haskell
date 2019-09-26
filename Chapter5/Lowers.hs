module Chapter5.Lowers where

import           Data.Char

lowers :: String -> Int
lowers xs = length [ x | x <- xs, isAsciiLower x ]
