{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Main where

import AOC
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Debug.Trace (traceShow)
import qualified Data.Map.Strict as M

import Test.HUnit hiding (show)
import Data.Maybe (fromJust)
import Data.ByteString (find)
import Control.Applicative (liftA2)

main :: IO ()
main = do
  AOC.runStringSolution (AOC.Solution
    { dayNum=9
    , solver=
      [ AOC.ParseSolver reportP solve1
      , AOC.ParseSolver reportP solve2
      ]
    , testWants=["114","2"]
    })

solve1 :: [[Int]] -> String
solve1 = show . sum . map findNext

solve2 :: [[Int]] -> String
solve2 = show . sum . map findPrevious

findNext :: [Int] -> Int
findNext = extrapolate . buildDiffTower

findPrevious :: [Int] -> Int
findPrevious = findNext . reverse

extrapolate :: [[Int]] -> Int
extrapolate [xs]
  | all (==0) xs = 0
  | otherwise    = error "Last layer is not all 0"
extrapolate (xs:xss) =
  let newDiff = extrapolate xss
   in last xs + newDiff

buildDiffTower :: [Int] -> [[Int]]
buildDiffTower xs
  | all (==0) xs = [xs]
  | otherwise    = xs : buildDiffTower (findDiffs xs)

findDiffs :: [Int] -> [Int]
findDiffs xs = zipWith (-) (tail xs) xs


reportP :: Parser [[Int]]
reportP = sepEndBy1 historyP newline <* eof

historyP :: Parser [Int]
historyP = sepBy1 numberP (many1 $ string " ")

numberP :: Parser Int
numberP = read <$> liftA2 (++) (string "-" <|> pure "") (many1 digit)