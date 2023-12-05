{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (show, read)
import qualified Prelude

import AOC
import Data.Bifunctor (first)
import qualified Data.Text.Read as T
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Maybe

read :: Read a => T.Text -> a
read = Prelude.read . T.unpack

show :: Show a => a -> T.Text
show = T.pack . Prelude.show

main :: IO ()
main = AOC.runTextSolution (AOC.Solution
  { dayNum=2
  , solver=
    [ AOC.LineSolver process1 (show . sum)
    , AOC.LineSolver process2 (show . sum)
    ]
  , testWants=["8", "2286"]
  })


extractGame :: T.Text -> (Int, [(T.Text, Int)])
extractGame line =
  let
    [game, rest1] = T.splitOn ": " line
    gameId = (read . (!!1) . T.splitOn " " $ game) :: Int
    draws = map ((\[a,b] -> (b, read a)) . T.splitOn " ") . concatMap (T.splitOn ", ") $ T.splitOn "; " rest1
  in (gameId, draws)

process1 :: T.Text -> Int
process1 line =
  let
    inventory = M.fromList [("red", 12),("green",13),("blue",14)]

    (gameId, draws) = extractGame line

    isValid :: (T.Text, Int) -> Bool
    isValid (key, max) = fromMaybe 0 (M.lookup key inventory) >= max
  in
    if all isValid draws then gameId else 0

process2 :: T.Text -> Int
process2 line =
  let
    (gameId, draws) = extractGame line
    inventory = M.fromListWith max draws
  in
    product $ M.elems inventory
