module Practice where
import           Hangman                        ( getCh )
import           Nim                            ( getDigit )

-- 1
putStr' :: String -> IO ()
putStr' [] = return ()
putStr' xs = sequence_ [ putChar x | x <- xs ]

-- 4
adder :: IO ()
adder = do
  n <- getDigit "How many numbers? "
  if n < 1
    then do
      print "Error: expected > 0"
      adder
    else do
      total <- adder' 0 n
      putStr "The total is"
      print $ show total

adder' :: Int -> Int -> IO Int
adder' total remaining = do
  n <- getDigit ""
  if remaining == 1
    then return (total + n)
    else adder' (total + n) (remaining - 1)

-- 5
adder'' :: IO ()
adder'' = do
  n <- getDigit "How many numbers? "
  if n < 1
    then do
      print "Error: expected > 0"
      adder
    else do
      total <- sequence (replicate n (getDigit ""))
      -- import Control.Monad (replicateM)
      -- total <- replicateM n (getDigit "")
      putStr "The total is "
      print $ sum total


-- 6
safeInit :: [a] -> [a]
safeInit []  = []
safeInit [_] = []
safeInit xs  = init xs

readLine :: IO String
readLine = readLine' []

readLine' :: String -> IO String
readLine' xs = do
  x <- getCh
  case x of
    '\n' -> do
      putChar x
      return xs
    '\DEL' -> do
      putChar '\b'
      putChar ' '
      putChar '\b'
      readLine' $ safeInit xs
    _ -> do
      putChar x
      readLine' (xs ++ [x])
