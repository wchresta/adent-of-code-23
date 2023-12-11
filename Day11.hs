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
import Data.Ix

import Test.HUnit hiding (show)
import Control.Applicative (liftA2)
import qualified Data.List
import Data.Foldable (foldl')

main :: IO ()
main = do
    AOC.runStringSolution (AOC.Solution
        { dayNum=11
        , solver=
            [ AOC.ParseSolver imageP $ solve 2
            , AOC.ParseSolver imageP $ solve 1_000_000
            ]
        , testWants=["374","82000210"]
        })

type Idx = (Int, Int)
type Image = M.Map Idx Char

solve :: Int -> [[Char]] -> String
solve e imgList =
  let
    h = length imgList
    w = length . head $ imgList

    img = toImage imgList
    (is, js) = foldl' (\(i,j) (is,js) -> (S.insert is i, S.insert js j))
                (S.empty, S.empty) $ M.keys img
    missI = S.fromAscList [0..h-1] `S.difference` is
    missJ = S.fromAscList [0..w-1] `S.difference` js
    expand_ x xs = x + (e - 1) * length [ 1 | t <- S.toList xs, t < x ]
    expanded = M.mapKeys (\(i,j) -> (expand_ i missI, expand_ j missJ)) img

    dist ((x,y),(u,v)) = abs (x-u) + abs (y-v)
    iPairs = pairs . M.keys $ expanded
   in show . sum . map dist $ iPairs
  
toImage :: [[Char]] -> Image
toImage = M.fromList . filter (('.' /=) . snd) .  AOC.indexify

imageP :: Parser [[Char]]
imageP = sepEndBy1 (many1 $ oneOf ".#") newline
