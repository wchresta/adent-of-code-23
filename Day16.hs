module Main where

import AOC
import Text.Parsec hiding ((<|>))
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Debug.Trace (trace, traceShow, traceShowId)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Bifunctor
import Data.Maybe
import Data.Ix
import Data.Function.Memoize
import Control.Parallel.Strategies

import Test.HUnit hiding (show)
import Control.Applicative (liftA2)
import Data.List
import Data.Foldable (foldl')
import Control.Parallel
import Control.Monad.Fix (fix)
import Data.Tuple (swap)
import Data.Char (ord)
import Control.DeepSeq

main :: IO ()
main = do
    AOC.runStringSolution (AOC.Solution
        { dayNum=16
        , solver=
            [ AOC.ParseSolver gridP solve1
            , AOC.ParseSolver gridP solve2
            ]
        , testWants=["46", "51"]
        })

type Idx = (Int,Int)
data Grid = G Idx (M.Map Idx Char)

instance Show Grid where
  show (G xy m) = unlines $ mapToMatrix m xy '.'

solve1 :: [String] -> String
solve1 gs = show . countEnergized . fst $ lazorStepMems g S.empty [L (0,-1) E]
  where g = AOC.toGridWithIgnore G '.' gs

solve2 :: [String] -> String
solve2 gs = show . maximum . map f $ edges
  where g@(G (maxI, maxJ) _) = AOC.toGridWithIgnore G '.' gs

        f :: Lazor -> Int
        f = countEnergized . fst . lazorStepMems g S.empty . pure

        edges = 
          [ L (i,-1) E | i <- [0..maxI] ] ++
          [ L (i,maxJ+1) W | i <- [0..maxI] ] ++
          [ L (-1,j) S | j <- [0..maxJ] ] ++
          [ L (maxI+1,j) N | j <- [0..maxJ] ]



countEnergized :: Mem -> Int
countEnergized = S.size . S.map lPos

data Dir = N | E | S | W
  deriving (Show, Eq, Ord)

addDir :: Dir -> (Int, Int) -> (Int, Int)
addDir N (x,y) = (x-1,y)
addDir E (x,y) = (x,y+1)
addDir S (x,y) = (x+1,y)
addDir W (x,y) = (x,y-1)

data Lazor = L { lPos :: (Int, Int), lDir :: Dir }
  deriving (Show, Eq, Ord)

type Mem = S.Set Lazor

lazorStepMems :: Grid -> Mem -> [Lazor] -> (Mem, [Lazor])
lazorStepMems g m [] = (m, [])
lazorStepMems g m ls = let (m', ls') = lazorStepMem g m ls in lazorStepMems g m' ls'

lazorStepMem :: Grid -> Mem -> [Lazor] -> (Mem, [Lazor])
lazorStepMem g@(G (maxI,maxJ) _) m ls = (newMem, nls)
  where
    nls = concatMap (filter legal . filter (`S.notMember` m) . lazorStep g) ls
    newMem = S.union m (S.fromList nls)
    legal (L (x,y) _) = x >= 0 && y >= 0 && x <= maxI && y <= maxJ

lazorStep :: Grid -> Lazor -> [Lazor]
lazorStep g@(G (maxI,maxJ) m) l@(L xy d) =
    case next of
      Nothing -> [L nextXY d]
      Just m -> handleMirror d m
  where
    nextXY = addDir d xy
    next = M.lookup nextXY m

    handleMirror E '-' = [L nextXY E]
    handleMirror E '\\' = [L nextXY S]
    handleMirror E '/' = [L nextXY N]
    handleMirror E '|' = [L nextXY S, L nextXY N]

    handleMirror W '-' = [L nextXY W]
    handleMirror W '\\' = [L nextXY N]
    handleMirror W '/' = [L nextXY S]
    handleMirror W '|' = [L nextXY N, L nextXY S]

    handleMirror N '-' = [L nextXY W, L nextXY E]
    handleMirror N '\\' = [L nextXY W]
    handleMirror N '/' = [L nextXY E]
    handleMirror N '|' = [L nextXY N]

    handleMirror S '-' = [L nextXY W, L nextXY E]
    handleMirror S '\\' = [L nextXY E]
    handleMirror S '/' = [L nextXY W]
    handleMirror S '|' = [L nextXY S]

    handleMirror x y = error $ show (x,y)

gridP :: Parser [String]
gridP = many1 $ many1 (oneOf ".|-\\/") <* newline