module Main where

import AOC
import Data.Bifunctor (first)
import Data.Char (isDigit, digitToInt)
import Data.List (isPrefixOf, isSuffixOf)

main :: IO ()
main = AOC.runStringSolution (AOC.Solution
  { dayNum=1
  , solver=
    [ AOC.LineSolver process1 (show . sum)
    , AOC.LineSolver process2 (show . sum)
    ]
  , testWants=["142", "281"]
  })

process1 :: String -> Int
process1 line = last digits + 10 * head digits
  where digits = map digitToInt . filter isDigit $ line

process2 :: String -> Int
process2 line = findRight line + 10 * findLeft line


findLeft = find matches
findRight = find mirrorMatches . reverse

find :: [(String,Int)] -> String -> Int
find m s = if any snd res
   then fst . head . filter snd $ res
   else find m (tail s)
  where
    res = map (\(a,i) -> (i, a `isPrefixOf` s)) m

mirrorMatches = map (first reverse) matches

matches :: [(String, Int)]
matches =
  [ ("one", 1)
  , ("two", 2)
  , ("three", 3)
  , ("four", 4)
  , ("five", 5)
  , ("six", 6)
  , ("seven", 7)
  , ("eight", 8)
  , ("nine", 9)
  ] ++ map (\c -> (show c, c)) [1..9]
