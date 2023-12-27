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
            [ AOC.parseSolver gridP solve1 [("test1","39")]
            , AOC.parseSolver gridP (solve2 26501365) []
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
  = show .  S.size . reachableFromS g $ n
  where n = 6

solve1 :: Grid -> String
solve1 g
  = show . S.size
  . reachableFromS g $ n
  where n = 64

solve2 :: Int -> Grid -> String
solve2 n g
  = show totNum
  where
    cornerStarts =
        [ (0,0) -- NW
        , (0,cubeSide-1) -- NE
        , (cubeSide-1,cubeSide-1) -- SE
        , (cubeSide-1,0) -- SW
        ]

    sideStarts =
        [ (0,midIdx) -- N
        , (midIdx,cubeSide-1) -- E
        , (cubeSide-1,midIdx) -- S
        , (midIdx,0) -- W
        ]
    
    {-
    C = Corner
    OC = OffCorner
    S = Side
    EF = EvenFill
    OF = OddFill

    cubeNums(even)
    |        cubeNums-1 (odd)
    |        |        ...
             C[SE]  OC[SE]    0
    C[SE]   OC[SE]  OF        |
    S[E]    OF      EF      Start
    C[NE]   OC[NE]  OF
             C[NE]  OC[NE]
                  ..    C  OC OF OC C
                           C  S  C
    
        EvenFills OddFills
    M   :   0      0
    M-1 :   0      1
    M-2 :   1      2
    M-3 :   2      3
    M-4 :   3      4
    ...
    0   :   M-1    M
    ...

    In Total:

    EvenFills: (M-1)*(M-2) + M-1 = (M-1)^2
    OddFills:      M*(M-1) + M   = M^2

    -}

    m = cubeNums
    totNum 
      = sideNums
      + m*cornNums
      + (m-1) * offCornNums
      + (m-1)^2 * evenFillNum
      + m^2 * oddFillNum

    evenFill = reachable g (0,0) (cubeSide * 2 + sideFuel `mod` 2)
    oddFill = reachable g (0,0) (cubeSide * 2 + sideFuel `mod` 2 + 1)

    corners = map (\start -> reachable g start cornerFuel) cornerStarts
    offCorners = map (\start -> reachable g start (cornerFuel+cubeSide)) cornerStarts
       

    sides = map (\start -> reachable g start sideFuel) sideStarts

    cornNums = sum . map S.size $ corners
    offCornNums = sum . map S.size $ offCorners
    sideNums = sum . map S.size $ sides
    evenFillNum = S.size evenFill
    oddFillNum = S.size oddFill

    -- We save a whole cube, but need to pay half a cube to get
    -- to the corner instead of the side.
    cornerFuel = sideFuel - midIdx

    -- Turns out, our fuel is *exactly* enough to fill the last
    -- cube. This assumption is important; so we ensure it here.
    (cubeNums, 0) = (n-midIdx) `divMod` cubeSide
    sideFuel = cubeSide-1

    midIdx = cubeSide `div` 2
    cubeSide = fst (gDim g) + 1

reachableFromS :: Grid -> Int -> S.Set Idx
reachableFromS g = reachable g start
  where
    start = head . M.keys . M.filter (=='S') . gMap $ g

reachable :: Grid -> Idx -> Int -> S.Set Idx
reachable g start n
  = S.filter (\(x,y) -> (x + y) `mod` 2 /= n `mod` 2)
  . explore n S.empty
  . S.singleton
  $ start
  where
    rockIdx :: S.Set Idx
    rockIdx = S.fromList . M.keys . M.filter (=='#') . gMap $ g

    findNeighbours :: Idx -> S.Set Idx
    findNeighbours idx = star (gDim g) idx `S.difference` rockIdx

    explore :: Int -> S.Set Idx -> S.Set Idx -> S.Set Idx
    explore 0 visited as = visited `S.union` as
    explore n visited as
      | S.null as = visited
      | otherwise = explore (n-1) (visited `S.union` as) newEdges
      where
        newEdges = mconcat (S.toList $ S.map findNeighbours as) `S.difference` visited

gridP :: Parser Grid
gridP = toGrid Grid <$> sepEndBy (many1 $ oneOf "S.#") newline <* eof
