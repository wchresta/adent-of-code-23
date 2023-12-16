{-# LANGUAGE DeriveAnyClass, TemplateHaskell #-}
module Main where

import AOC
import Text.Parsec hiding ((<|>))
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Debug.Trace (trace, traceShow)
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
import Control.DeepSeq

main :: IO ()
main = do
    AOC.runStringSolution (AOC.Solution
        { dayNum=14
        , solver=
            [ AOC.ParseSolver platformP solve1
            , AOC.ParseSolver platformP solve2
            ]
        , testWants=["136"]
        })



solve1 :: Platform -> String
solve1 = show . calcWeight . slideNorth

solve2 :: Platform -> String
solve2 p0 = 
  let 
    p1000 = slideNCycles 1000 p0
    n = traceShow p1000 $ findCycle p1000
    rest = (1000000000 - 1000) `mod` n
  in
    show . calcWeight . slideNCycles rest $ p1000

type Idx = (Int,Int)
data Platform = P Idx !(M.Map Idx Char)
  deriving (Eq)

instance Show Platform where
  show (P mij m) = unlines $ AOC.mapToMatrix m mij '.'
  
calcWeight :: Platform -> Int
calcWeight (P (mi,mj) m) = M.foldrWithKey' addW 0 m
  where
    addW :: Idx -> Char -> Int -> Int
    addW (i,j) 'O' t = t + (mi-i+1)
    addW _     _   t = t

slideNorth :: Platform -> Platform
slideNorth (P (mi,mj) m) =
  let

    dropNewStone m i j = d i
      where
        d 0 = M.insert (0,j) 'O' m
        d i = if (i-1,j) `M.member` m
                then M.insert (i,j) 'O' m
                else d (i-1)

    dropLine ma i =
       foldl (\m j ->
            if Just 'O' == M.lookup (i,j) m
              then dropNewStone (M.delete (i,j) m) i j
              else m) ma [0..mj]
   in P (mi,mj) $ foldl dropLine m [1..mi]

applyTransformed :: (Idx -> Idx) -> (Idx -> Idx) -> (Platform -> Platform) -> Platform -> Platform
applyTransformed trans back f (P ij m) = P ij (M.mapKeys back appliedM)
  where
    P _ appliedM = f $ P ij (M.mapKeys trans m)

slideSouth :: Platform -> Platform
slideSouth p@(P (mi,_) _) =
  applyTransformed (first (mi -)) (first (mi -)) slideNorth p

slideWest :: Platform -> Platform
slideWest p@(P (mi,_) _) =
  applyTransformed swap swap slideNorth p

slideEast :: Platform -> Platform
slideEast p@(P (mi,mj) _) =
  applyTransformed (\(i,j) -> (mj-j,i)) (\(j,i) -> (i,mj-j)) slideNorth p

slideCycle :: Platform -> Platform
slideCycle = slideEast . slideSouth . slideWest . slideNorth

slideNCycles :: Int -> Platform -> Platform
slideNCycles 0 p = p
slideNCycles n p = slideNCycles (n-1) $ slideCycle p

findCycle :: Platform -> Int
findCycle p0 = f 1 $ slideCycle p0
  where
    f n p | p == p0   = n
    f 10000 p = error "No cycle found :("
    f n p | otherwise = f (n+1) (slideCycle p)

platformP :: Parser Platform
platformP = do
  cells <- many1 (many1 (oneOf ".#O") <* newline)
  let
    width = length . head $ cells
    height = length cells
    mp = AOC.matrixToMap cells
  pure $ P (height-1,width-1) . M.filter ('.'/=) $ mp
