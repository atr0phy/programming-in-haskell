import           Data.Char
import           Data.List
import           System.IO
import           System.Random           hiding ( next )

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  conditions <- getDigit "How many marks for win? "
  player     <- promptYesNo "Go first? (y/n)"
  play (empty conditions) player

size :: Int
size = 3

depth :: Int
depth = 9

type Grid = [[Player]]

data Player = O | B | X deriving (Eq, Ord, Show)

next :: Player -> Player
next O = X
next B = B
next X = O

-- empty :: Grid
-- empty = replicate size (replicate size B)
-- 4
empty :: Int -> Grid
empty n = replicate n (replicate n B)

full :: Grid -> Bool
full = notElem B . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
 where
  os = length (filter (== O) ps)
  xs = length (filter (== X) ps)
  ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
 where
  line = all (== p)
  rows = g
  cols = transpose g
  dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [ g !! n !! n | n <- [0 .. size - 1] ]

won :: Grid -> Bool
won g = wins O g || wins X g

putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
  where bar = [replicate ((size * 4) - 1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
 where
  beside = foldr1 (zipWith (++))
  bar    = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave _ []       = []
interleave _ [y     ] = [y]
interleave x (y : ys) = y : x : interleave x ys

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size ^ 2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p = if valid g i then [chop size (xs ++ [p] ++ ys)] else []
  where (xs, B : ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat _prompt = do
  putStr _prompt
  xs <- getLine
  if xs /= [] && all isDigit xs
    then return (read xs)
    else do
      putStrLn "ERROR: Invalid number"
      getNat _prompt

tictactoe :: IO ()
tictactoe = run (empty 3) O

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: (Int, Int) -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

run :: Grid -> Player -> IO ()
run g p = do
  cls
  goto (1, 1)
  putGrid g
  run' g p

run' :: Grid -> Player -> IO ()
run' g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g = putStrLn "It's a draw!\n"
  | otherwise = do
    i <- getNat (prompt p)
    case move g i p of
      [] -> do
        putStrLn "ERROR: Invalid move"
        run' g p
      [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player" ++ show p ++ ", enter your move: "

data Tree a = Node a [Tree a] deriving Show

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [ gametree g' (next p) | g' <- moves g p ]

moves :: Grid -> Player -> [Grid]
moves g p | won g     = []
          | full g    = []
          | otherwise = concat [ move g i p | i <- [0 .. ((size ^ 2) - 1)] ]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _ ) = Node x []
prune n (Node x ts) = Node x [ prune (n - 1) t | t <- ts ]

minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g []) | wins O g  = Node (g, O) []
                    | wins X g  = Node (g, X) []
                    | otherwise = Node (g, B) []
minimax (Node g ts) | turn g == O = Node (g, minimum ps) ts'
                    | turn g == X = Node (g, maximum ps) ts'
 where
  ts' = map minimax ts
  ps  = [ p | Node (_, p) _ <- ts' ]

-- 2
bestmoves :: Grid -> Player -> [Grid]
bestmoves g p = [ g' | Node (g', p') _ <- ts, p' == best ]
 where
  tree              = prune depth (gametree g p)
  Node (_, best) ts = minimax tree

-- 3
bestmove :: Grid -> Player -> Grid
bestmove g p = head [ g' | Node (g', p') _ <- sortOn treeDepth ts, p' == best ]
 where
  tree              = gametree g p
  tree'             = prune (mydepth tree) tree
  Node (_, best) ts = minimax tree'

play :: Grid -> Player -> IO ()
play g p = do
  cls
  goto (1, 1)
  putGrid g
  play' g p

play' :: Grid -> Player -> IO ()
play' g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g = putStrLn "It's a draw!\n"
  | p == O = do
    i <- getNat (prompt p)
    case move g i p of
      [] -> do
        putStrLn "ERROR: Invalid move"
        play' g p
      [g'] -> play g' (next p)
  | p == X = do
    putStr "Player X is thinking...."
    -- 2
    -- let gs = bestmoves g p
    -- n <- randomRIO (0, length gs - 1)
    -- play (gs !! n) (next p)
    -- 3
    play (bestmove g p) (next p)

-- 1
-- let tree = gametree empty O
nodes :: Tree a -> Int
nodes (Node _ ts) = 1 + sum (map nodes ts)

mydepth :: Tree a -> Int
mydepth (Node _ []) = 0
mydepth (Node _ ts) = 1 + maximum (map mydepth ts)

-- 3
treeDepth :: Tree a -> Int
treeDepth (Node _ []) = 0
treeDepth (Node _ ts) = 1 + minimum (map treeDepth ts)

-- 4a
newline :: IO ()
newline = putChar '\n'

promptYesNo :: String -> IO Player
promptYesNo prompt = do
  putStr prompt
  ans <- getChar
  newline
  if ans == 'y'
    then return O
    else if ans == 'n'
      then return X
      else do
        putStrLn "ERROR: Please input 'y' or 'n'"
        promptYesNo prompt

-- 4b
getDigit :: String -> IO Int
getDigit prompt = do
  putStr prompt
  x <- getChar
  newline
  if isDigit x
    then return (digitToInt x)
    else do
      putStrLn "ERROR: Invalid digit"
      getDigit prompt
