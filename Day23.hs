module Main where

import AOC
import Text.Parsec hiding ((<|>), State(..), getState, lower)
import Text.Parsec.String
import Text.Parsec.Char hiding (lower)
import Text.Parsec.Combinator
import Debug.Trace (trace, traceShow, traceShowId, traceStack)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Numeric (readHex)
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Foldable (foldl', maximumBy)
import Data.List (transpose, sortOn, nub, intercalate)
import Control.Monad.State.Lazy
import Data.Containers.ListUtils (nubOrd)
import Data.Biapplicative (bimap)
import Data.Function ((&))
import Data.Foldable1 (foldl1')
import Data.Bifunctor (second)
import qualified Data.IntPSQ as Q

main :: IO ()
main = do
    AOC.runSolverWithTests (AOC.SolutionWithTests
        { dayNums=23
        , solvers=
            [ AOC.parseSolver gridP solve1 [("test1", "94")]
            , AOC.parseSolver gridP solve2 [("test1", "154")]
            ]
        })


type Idx = (Int, Int)

data Grid = Grid { gDim :: (Int, Int), gMap :: M.Map Idx Char }
  deriving (Eq, Ord)

instance Show Grid where
  show g = "\n" ++ unlines (AOC.mapToMatrix (gMap g) (gDim g) '.')

solve1 :: Grid -> String
solve1 g = show . maximum . map ((+1) . S.size) $ walk S.empty (0,0)
  where
    goal = gDim g

    star (i,j) = case M.lookup (i,j) (gMap g) of
      Just '^' -> S.singleton (i-1,j)
      Just '>' -> S.singleton (i,j+1)
      Just 'v' -> S.singleton (i+1,j)
      Just '<' -> S.singleton (i,j-1)
      Just '#' -> S.empty
      Just '.' -> S.filter (('#' /=) . fromMaybe '#' . flip M.lookup (gMap g)) $
                    S.fromList [(i+1,j),(i-1,j),(i,j-1),(i,j+1)]
      Nothing -> S.empty

    walk :: S.Set Idx -> Idx -> [S.Set Idx]
    walk visited pos
     | pos == goal = [S.insert goal visited]
     | otherwise   = concatMap (walk (S.insert pos visited)) (star pos `S.difference` visited)

solve2 :: Grid -> String
solve2 g = show . (+2) $ findLongestWalk discoverEdges
    --dotGraph discoverEdges  
  where
    m = gMap g
    goal = gDim g

    star (i,j) = S.filter (('#' /=) . fromMaybe '#' . flip M.lookup m) $
                        S.fromList [(i+1,j),(i-1,j),(i,j-1),(i,j+1)]

    next prev = S.elemAt 0 . S.delete prev . star

    findLongestWalk :: M.Map Idx (S.Set (Idx, Int)) -> Int
    findLongestWalk m = fromJust $ f (S.singleton (0,0)) (0,0)
      where
        f :: S.Set Idx -> Idx -> Maybe Int
        f seen e | e == goal = Just 0
        f seen e =
            (\xs -> if null xs then Nothing else Just (maximum xs))
            . mapMaybe (\(e', len) ->
                if e' `S.notMember` seen
                  then (len +) <$> f (S.insert e' seen) e'
                  else Nothing)
            $ S.toList $ fromJust (
              M.lookup e m)

    discoverEdges :: M.Map Idx (S.Set (Idx, Int))
    discoverEdges = M.fromListWith S.union . map (second S.singleton) $ walk S.empty [((0,0),(0,1))] []
      where
        walk :: S.Set Idx -> [(Idx,Idx)] -> [(Idx, (Idx, Int))] -> [(Idx, (Idx, Int))]
        walk visited [] found = found
        walk visited ((junction,inHole):js) found =
             walk newVisited (js ++ newJs) newFound
          where
            (newJunction, path, outHole) = walkEdge junction inHole
            newFound = (junction, (newJunction, path)):(newJunction, (junction, path)):found
            newEdges = S.delete outHole (star newJunction) `S.difference` visited
            newVisited = S.insert outHole $ S.union visited newEdges
            newJs = map (newJunction,) . S.toList $ newEdges

    walkEdge :: Idx -> Idx -> (Idx, Int, Idx)
    walkEdge = f 1
      where
        f 1 prev pos = f 2 pos (next prev pos)  -- Ignore the entry hole
        f len prev pos | pos == goal = (goal, len, prev)
        f len prev pos = case M.lookup pos m of
          Just '.' -> f (len+1) pos (next prev pos)
          _        -> (next prev pos, len+1, pos)


dotGraph :: M.Map Idx (S.Set (Idx, Int)) -> String
dotGraph m = unlines $
      ["\ngraph {"] ++ 
      [ "  \"" ++ show f ++ "\" -- \"" ++ show t ++ "\" " ++ attrib len
        | (f@(x,y),ts) <- M.assocs m
        , (t@(a,b),len) <- S.toList ts
        , f < t ] ++
      ["}"]
  where
    attrib len = "[label=\"" ++ show len ++ "\",weight=" ++ show (501-len) ++ "," ++ col len ++ "]"
    col len
      | len < 100 = "color=grey,penwidth=0.5"
      | len < 200 = "color=black"
      | len < 300 = "color=orange"
      | otherwise = "color=red,penwidth=2.0"

gridP :: Parser Grid
gridP = AOC.toGrid Grid . map middle . middle <$> sepEndBy1 (many1 $ oneOf ".#^>v<") newline
  where
    middle = tail . init