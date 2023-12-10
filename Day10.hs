{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Main where

import AOC
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Debug.Trace (trace, traceShow)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe ( fromMaybe, fromJust )
import Data.Either ( fromRight )

import Test.HUnit hiding (show)
import Control.Applicative (liftA2)
import qualified Data.List

main :: IO ()
main = do
  c <- tests
  if errors c > 0 || failures c > 0
    then putStrLn "** Some unit tests failed **"
    else
      AOC.runStringSolution (AOC.Solution
        { dayNum=10
        , solver=
          [ AOC.ParseSolver mazeP solve1
          , AOC.ParseSolver mazeP solve2
          ]
        , testWants=["8","10"]
        })

type Idx = (Int, Int)
type Maze = M.Map Idx Pipe

data Pipe = P Bool Bool Bool Bool
  deriving (Eq)

data Dir = N | E | S | W
  deriving (Show, Eq)

solve1 :: Maze -> String
solve1 m = show $ run m 1 left right
  where
    -- O(n) to find start
    start = fst . fromJust . Data.List.find (\(idx, P a b c d) -> and [a,b,c,d]) . M.assocs $ m
    [left, right] = canGo m start

run :: Maze -> Int -> (Dir, Idx) -> (Dir, Idx) -> Int
run m t l r
   | snd l == snd r    = t
   | otherwise = run m (t+1) (walk m l) (walk m r)

walk :: Maze -> (Dir, Idx) -> (Dir, Idx)
walk m (dir, idx@(i,j)) =
  let
    P n e s w = fromJust $ M.lookup idx m

    go d True _ _ _ | d /= S = (N, (i-1,j))
    go d _ True _ _ | d /= W = (E, (i,j+1))
    go d _ _ True _ | d /= N = (S, (i+1,j))
    go d _ _ _ True | d /= E = (W, (i,j-1))
   in go dir n e s w

canGo :: Maze -> Idx -> [(Dir, Idx)]
canGo m idx@(i,j) =
  let
    P n e s w = fromJust $ M.lookup idx m

    check :: Dir -> Bool -> [(Dir, Idx)]
    check _ False = []
    check N True = case M.lookup (i-1,j) m of
                    Just (P _ _ True _) -> [(N, (i-1,j))]
                    _    -> []
    check E True = case M.lookup (i,j+1) m of
                    Just (P _ _ _ True) -> [(E, (i,j+1))]
                    _    -> []
    check S True = case M.lookup (i+1,j) m of
                    Just (P True _ _ _) -> [(S, (i+1,j))]
                    _    -> []
    check W True = case M.lookup (i,j-1) m of
                    Just (P _ True _ _) -> [(W, (i,j-1))]
                    _    -> []
  in
    check N n ++ check E e ++ check S s ++ check W w

-----

solve2 :: Maze -> String
solve2 m = show . sum . concatMap (map (\x -> if x then 1 else 0)) $ inout
  where
    -- O(n) to find start
    start@(si,sj) = fst . fromJust . Data.List.find (\(idx, P a b c d) -> and [a,b,c,d]) . M.assocs $ m
    [left@(ld,_), right@(rd,_)] = canGo m start

    -- S is messing us up; replace S with the appropriate pipe
    newS = P (ld == N || rd == N) (ld == E || rd == E) (ld == S || rd == S) (ld == W || rd == W)
    cleanStart = M.insert start newS m

    theLoop = S.fromList $ findLoop cleanStart [start] left right
    cleanMaze = M.fromList [ (idx, p) | (idx, p) <- M.assocs cleanStart, idx `S.member` theLoop ]
    cleanList = mazeList cleanMaze
    inout = map markInOut cleanList


mazeList :: Maze -> [[Pipe]]
mazeList m = [ [ fromMaybe empty $ M.lookup (i,j) m | j <- [0..w] ] | i <- [0..h] ]
  where
    w = maximum . map snd . M.keys $ m
    h = maximum . map fst . M.keys $ m


findLoop :: Maze -> [Idx] -> (Dir, Idx) -> (Dir, Idx) -> [Idx]
findLoop m t l r
   | snd l == snd r    = snd l:t
   | otherwise         = findLoop m (snd l:snd r:t) (walk m l) (walk m r)

{-
  To count InOut, we can check if we are crossing a pipe going from left to right
  We'll remember the parity of going up or down
  | -> 1,1 -> In
  || -> 0,0 -> Out
  LJ -> 0,0 -> Out
  L7 -> 1,1 -> In
  So if the parity is both 0, we're out, if the parity is both 1, we're in
  If the parity is not the same, we're ON a pipe

  L--J.L7...LJS7F-7L7.
  OOOOOOOIIIOOOOOOOOOO
  01112233333456666677
  00000001111112344556
-}
markInOut :: [Pipe] -> [Bool]
markInOut = f False False
  where
    f x y []             = []
    f x y (P False False False False:ps) = (x && y):f x y ps
    f x y (P u _ d _:ps)                 = False:f (u /= x) (d /= y) ps

---  Parser

mazeP :: Parser Maze
mazeP = mazify <$> sepEndBy1 (many1 pipeP) newline <* eof

pipeP :: Parser Pipe
pipeP = do
  c <- oneOf ".SFJ|L7-"
  pure $ case c of
    '.' -> P False False False False
    'S' -> P True True True True
    'F' -> P False True True False
    'J' -> P True False False True
    '|' -> P True False True False
    'L' -> P True True False False
    '7' -> P False False True True
    '-' -> P False True False True
    _   -> error "Unknown pipe"

mazify :: [[Pipe]] -> Maze
mazify = M.fromList . concatMap (\(i,line) -> [ ((i,j),p) | (j,p) <- zip [0..] line ] ) . zip [0..]


--- Testing

showInOut :: [Bool] -> String
showInOut = map (\b -> if b then 'I' else ' ')

p :: String -> [Pipe]
p s = let Right ps = parse (many1 pipeP <* eof) "" s in ps

tests :: IO Counts
tests = runTestTT . TestList $
  [ TestCase $ assertEqual "" "     " (showInOut . markInOut $ p ".....")
  , TestCase $ assertEqual "" "  I  " (showInOut . markInOut $ p ".|.|.")
  , TestCase $ assertEqual "" "     " (showInOut . markInOut $ p ".||..")
  , TestCase $ assertEqual "" " III " (showInOut . markInOut $ p "|...|")
  , TestCase $ assertEqual "" "     " (showInOut . markInOut $ p "LJ...")
  , TestCase $ assertEqual "" "   II" (showInOut . markInOut $ p "LJ|..")
  , TestCase $ assertEqual "" "  III" (showInOut . markInOut $ p "L7...")
  , TestCase $ assertEqual "" "  I   " (showInOut . markInOut $ p "L7.L7.")
  , TestCase $ assertEqual "" "      " (showInOut . markInOut $ p "F7.F7.")
  , TestCase $ assertEqual "" "  I  I" (showInOut . markInOut $ p "FJ.F7.")
  , TestCase $ assertEqual "" "  I   " (showInOut . markInOut $ p "FJ.FJ.")
  , TestCase $ assertEqual "" "     I" (showInOut . markInOut $ p "F7.FJ.")
  , TestCase $ assertEqual "" "      I" (showInOut . markInOut $ p "F-7.FJ.")
  , TestCase $ assertEqual "" "       I" (showInOut . markInOut $ p "F-7.F-J.")
  , TestCase $ assertEqual "" "    I    " (showInOut . markInOut $ p "F-7|.F-J.")
  ]

--- Debugging

showMaze :: Maze -> String
showMaze = unlines . map (concatMap show) . mazeList

empty = P False False False False

instance Show Pipe where
    show (P False False False False) = "."
    show (P True True True True    ) = "S"
    show (P False True True False  ) = "F"
    show (P True False False True  ) = "J"
    show (P True False True False  ) = "|"
    show (P True True False False  ) = "L"
    show (P False False True True  ) = "7"
    show (P False True False True  ) = "-"