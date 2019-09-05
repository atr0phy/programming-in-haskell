module BinaryConverter where

import           Data.Char

type Bit = Int

{-
Exp
bin2int [1,0,1,1]

1+2*(0+2*(1+2*(1+2*0)))
1+2*(0+2*(1+2*(1)))
1+2*(0+2*(3))
1+2*(6)
1+12 = 13
-}
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0
{-
bin2int bits = sum [ w * b | (w, b) <- zip weights bits ]
  where weights = iterate (* 2) 1
-}

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concatMap (addParityBit . make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 []   = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

chop9 :: [Bit] -> [[Bit]]
chop9 []   = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int . checkParityBit) . chop9

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

-- 7

computeParityBit :: [Bit] -> Bit
computeParityBit bits = sum bits `mod` 2

addParityBit :: [Bit] -> [Bit]
addParityBit bits = (computeParityBit bits) : bits

checkParityBit :: [Bit] -> [Bit]
checkParityBit [] = error "Parity Error"
checkParityBit (x : xs) | x == (computeParityBit xs) = xs
                        | otherwise               = error "Parity Error"

-- 8

unreliableChannel :: [Bit] -> [Bit]
unreliableChannel bits | null bits = []
                       | otherwise = tail bits

unreliableTransmit :: String -> String
unreliableTransmit = decode . unreliableChannel . encode
