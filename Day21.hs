module Main where

import AOC
import Text.Parsec hiding ((<|>), State(..), getState)
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Debug.Trace (trace, traceShow, traceShowId)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Numeric (readHex)
import Data.Maybe (fromJust)
import Data.Foldable (foldl')
import Data.Bifunctor (first)
import Data.List (transpose)


main :: IO ()
main = do
    AOC.runSolverWithTests (AOC.SolutionWithTests
        { dayNums=21
        , solvers=
            [ {-
            AOC.parseSolver gridP test1 
                [("test1", "...........\n"
                        ++ ".....###.#.\n"
                        ++ ".###.##.O#.\n"
                        ++ ".O#O#O.O#..\n"
                        ++ "O.O.#.#.O..\n"
                        ++ ".##O.O####.\n"
                        ++ ".##.O#O..#.\n"
                        ++ ".O.O.O.##..\n"
                        ++ ".##.#.####.\n"
                        ++ ".##O.##.##.\n"
                        ++ "...........\n")]
            , AOC.parseSolver gridP solve1 []
            
            , -} AOC.parseSolver gridP (solve2 5000) [("input", "16733044")]
            ]
        })


type Idx = (Int, Int)
data Grid = Grid { gDim :: (Int, Int), gMap :: M.Map Idx Char }

instance Show Grid where
  show (Grid hw g) = unlines $ AOC.mapToMatrix g hw '.'

star :: (Int,Int) -> Idx -> S.Set Idx
star hw (i,j) = S.filter (AOC.inGrid hw) $ S.fromList [ x | di <- [-1,1], dj <- [-1,1], x <- [(i,j+dj),(i+di,j)] ]

test1 :: Grid -> String
test1 g
  = debugIdx g
  . reachableFromS g $ n
  where n = 6

solve1 :: Grid -> String
solve1 g
  = show . S.size
  . reachableFromS g $ n
  where n = 64

solve2 :: Int -> Grid -> String
solve2 n g
  = unlines $
   (transpose . lines $ concatMap (debugIdx g) 
      [ evenFill
      , oddFill
      , evenFill
      ]) ++
   (transpose . lines $ concatMap (debugIdx g) 
      [ offCorners !! 3
      , evenFill
      , offCorners !! 0
      ]) ++
   (transpose . lines $ concatMap (debugIdx g) 
      [ corners !! 3
      , offSides !! 3
      , corners !! 0
      ])
      --, offCorners !! 0
      --, offCorners !! 3
  where
    cornerStarts = 
        [ (0,0) -- NW
        , (0,cubeSide) -- NE
        , (cubeSide,cubeSide) -- SE
        , (cubeSide,0) -- SW
        ]

    sideStarts = 
        [ (0,midIdx) -- N
        , (midIdx,cubeSide) -- E
        , (cubeSide,midIdx) -- S
        , (midIdx,0) -- W
        ]  

    evenFill = reachable g (0,0) (cubeSide * 2)
    oddFill = reachable g (0,0) (cubeSide * 2 + 1)

    corners = map (\start -> reachable g start cornerFuel) cornerStarts
    offCorners = map (\start -> reachable g start (cornerFuel+cubeSide)) cornerStarts
       

    sides = map (\start -> reachable g start sideFuel) sideStarts
    offSides = map (\start -> reachable g start (sideFuel+cubeSide)) sideStarts

    cornNums = sum . map S.size $ corners
    offCornNums = sum . map S.size $ offCorners
    sideNums = sum . map S.size $ sides
    offSideNums = sum . map S.size $ offSides

    -- We save a whole cube, but need to pay half a cube to get
    -- to the corner instead of the side.
    cornerFuel = sideFuel + cubeSide - midIdx + 1

    (cubeNums, sideFuel) = n `divMod` cubeSide
    midIdx = (cubeSide `div` 2) + 1
    cubeSide = fst (gDim g) + 1

reachableFromS :: Grid -> Int -> S.Set Idx
reachableFromS g = reachable g start
  where
    start = head . M.keys . M.filter (=='S') . gMap $ g

reachable :: Grid -> Idx -> Int -> S.Set Idx
reachable g start n
  = S.filter (\(x,y) -> (x + y) `mod` 2 == n `mod` 2)
  . explore n S.empty
  . S.singleton
  $ start
  where
    rockIdx :: S.Set Idx
    rockIdx = S.fromList . M.keys . M.filter (=='#') . gMap $ g

    findNeighbours :: Idx -> S.Set Idx
    findNeighbours idx = star (gDim g) idx `S.difference` rockIdx

    explore :: Int -> S.Set Idx -> S.Set Idx -> S.Set Idx
    explore (-1) visited _ = visited
    explore n visited as
      | S.null as = visited
      | otherwise = explore (n-1) (visited `S.union` as) newEdges
      where
        newEdges = mconcat (S.toList $ S.map findNeighbours as) `S.difference` visited

gridP :: Parser Grid
gridP = toGrid Grid <$> sepEndBy (many1 $ oneOf "S.#") newline <* eof

debugIdx g = unlines . (\w -> AOC.mapToMatrix w (gDim g) '.')
  . M.unionWith (\a b -> if b == '.' then a else b) (gMap g)
  . M.fromList
  . map (,'O')
  . S.toList
