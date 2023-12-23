{-# LANGUAGE DeriveAnyClass #-}
module Main where

import AOC
import Text.Parsec hiding ((<|>), State(..))
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

import Test.HUnit hiding (show, Path, State(..))
import Control.Applicative (liftA2)
import Data.List
import Data.Foldable (foldl', msum)
import Control.Parallel
import Control.Monad.Fix (fix)
import Data.Tuple (swap)
import Data.Char (ord)
import Control.DeepSeq
import qualified Data.OrdPSQ as Q
import Data.Hashable (Hashable, hashWithSalt)
import GHC.Generics (Generic)

main :: IO ()
main = do
    AOC.runSolverWithTests (AOC.SolutionWithTests
        { dayNums=17
        , solvers=
            [ AOC.WithTests (AOC.ParseSolver gridP (solve nextSteps1)) [("test1", "102")]
            , AOC.WithTests (AOC.ParseSolver gridP (solve nextSteps2)) [("test2", "94"), ("test3", "71")]
            ]
        })

type Idx = (Int, Int)

data Grid = G Idx (M.Map Idx Int)

instance Show Grid where
  show (G idx m) = unlines . (\x -> mapToMatrix x idx '.') . fmap (head . show) $ m


data Dir = N | E | S | W
  deriving (Show, Eq, Ord, Enum, Generic, Hashable)


data Result a = Impossible | Finished | NextSteps [a]

data Step = T { tIdx :: Idx, tDir :: Dir, tDirLen :: Int }
  deriving (Eq, Ord, Generic, Hashable)

instance Show Step where
  show (T (i,j) d l) = show (i,j) ++ show d ++ show l

type Queue = Q.OrdPSQ Step Int ()

solve :: (Grid -> Step -> [(Step, Int)]) -> [[Int]] -> String
solve nextSteps xs = 
    show . fst $ shortestRoute
  where
    g@(G (maxI,maxJ) _) = AOC.toGrid G xs

    isFinished (T (i,j) _ l) = (i,j) == (maxI, maxJ)

    shortestRoute :: (Int, [Step])
    shortestRoute =
      let (dists, prevs, end) = dijkstra
       in (fromJust $ M.lookup end dists, reverse $ reconstructRoute prevs end)
    
    reconstructRoute :: M.Map Step Step -> Step -> [Step]
    reconstructRoute m s = case M.lookup s m of
      Nothing -> []
      Just s' -> s:reconstructRoute m s'

    dijkstra =
      let
        (e, s) = (T (0,0) E 0, T (0,0) S 0)
        dists = M.fromList [(e,0),(s,0)]
        queue = Q.fromList [(s,0,()),(e,0,())]
        prevs = M.empty

        go q dists prevs = case Q.minView q of
          Nothing -> (dists, prevs, T (0,0) S 0)
          Just (u, _, _, q') ->
            if tIdx u == (maxI, maxJ)
              then (dists, prevs, u)
              else
                let
                  newEdges = nextSteps g u
                  (dists', prevs', q'') = foldl (updateQueue u) (dists, prevs, q') newEdges
                in go q'' dists' prevs'

        decreasePriority :: Step -> Int -> Queue -> Queue
        decreasePriority key prio queue =
          snd $ Q.alter (const ((), Just (prio, ()))) key queue

        updateQueue :: Step -> (M.Map Step Int, M.Map Step Step, Queue) -> (Step, Int) -> (M.Map Step Int, M.Map Step Step, Queue)
        updateQueue u (dists, prevs, queue) (v, cost) =
          let
            alt = fromJust (M.lookup u dists) + cost
           in case M.lookup v dists of
            Nothing -> (M.insert v alt dists, M.insert v u prevs, decreasePriority v alt queue)
            Just vDist -> if alt < vDist
              then (M.insert v alt dists, M.insert v u prevs, decreasePriority v alt queue)
              else (dists, prevs, queue)
       in
        go queue dists prevs



type Path = [Idx]

nextSteps1 = nextSteps candidateSteps1
nextSteps2 = nextSteps candidateSteps2

nextSteps :: (Grid -> Step -> [Step]) -> Grid -> Step -> [(Step, Int)]
nextSteps candidateSteps g@(G (maxI, maxJ) m) s = do
  let validate (T (i,j) _ _) = AOC.inGrid (maxI,maxJ) (i,j)
      addCost s@(T (i,j) _ _) = (s, fromMaybe 0 $ M.lookup (i,j) m)
  map addCost . filter validate . candidateSteps g $ s

dirVal :: Dir -> (Int, Int)
dirVal N = (-1,0)
dirVal S = (1,0)
dirVal E = (0,1)
dirVal W = (0,-1)

rotateCW :: Dir -> Dir
rotateCW N = E
rotateCW E = S
rotateCW S = W
rotateCW W = N

rotateCCW :: Dir -> Dir
rotateCCW N = W
rotateCCW W = S
rotateCCW S = E
rotateCCW E = N

addIdx :: (Int, Int) -> (Int, Int) -> (Int, Int)
addIdx (x,y) (a,b) = (a+x,b+y)

addDir :: Dir -> Step -> Step
addDir d (T ij d' n) = T (addIdx ij $ dirVal d) d (if d == d' then n+1 else 1)

candidateSteps1 :: Grid -> Step -> [Step]
candidateSteps1 _ s@(T _ d 3) = [addDir (rotateCW d) s, addDir (rotateCCW d) s]
candidateSteps1 _ s@(T _ d _) = [addDir (rotateCW d) s, addDir (rotateCCW d) s, addDir d s]

candidateSteps2 :: Grid -> Step -> [Step]
candidateSteps2 (G goal _) s@(T _ d l)
  | l < 3 = noGoal [addDir d s]
  | l == 3 = [addDir d s]
  | l >= 10 = noGoal [addDir (rotateCW d) s, addDir (rotateCCW d) s]
  | otherwise = addDir d s : noGoal [addDir (rotateCW d) s, addDir (rotateCCW d) s]
  where noGoal = filter ((/= goal) . tIdx) 



gridP :: Parser [[Int]]
gridP = many1 (many1 (read . pure <$> digit) <* newline)