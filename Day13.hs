{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Main where

import AOC
import Text.Parsec hiding ((<|>))
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Debug.Trace (trace, traceShow)
import Control.Applicative
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.List
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

main :: IO ()
main = do
    AOC.runStringSolution (AOC.Solution
        { dayNum=13
        , solver=
            [ AOC.ParseSolver puzzlesP (solve findMirror)
            , AOC.ParseSolver puzzlesP (solve findSmudged)
            ]
        , testWants=["405", "400"]
        })

solve :: ([String] -> Int) -> [[String]] -> String
solve solver = show . sum . map solver

findMirror :: [String] -> Int
findMirror xs = fromJust $ vert <|> (*100) <$> horiz
  where
    horiz = findDown [] . zip [1..] $ xs
    vert = findDown [] . zip [1..] . transpose $ xs


    findDown :: [String] -> [(Int, String)] -> Maybe Int
    findDown ups ((i,x):(j,y):xs)
         | x == y && and (zipWith (==) ups (map snd xs)) = Just i
    findDown ups ((_,d):ds) = findDown (d:ups) ds
    findDown ups downs = Nothing

findSmudged :: [String] -> Int
findSmudged xs = fromJust $ vert <|> (*100) <$> horiz
  where
    horiz = findDown [] . zip [1..] $ xs
    vert = findDown [] . zip [1..] . transpose $ xs

    countSmudges :: [String] -> [String] -> Int
    countSmudges as = sum . zipWith defects as

    findDown :: [String] -> [(Int, String)] -> Maybe Int
    findDown ups ((i,x):(j,y):xs)
         | 1 == defects x y && and (zipWith (==) ups (map snd xs)) = Just i
    findDown ups ((i,x):(j,y):xs)
         | x == y && 1 == countSmudges ups (map snd xs) = Just i
    findDown ups ((_,d):ds) = findDown (d:ups) ds
    findDown ups downs = Nothing

defects :: Eq a => [a] -> [a] -> Int
defects [] [] = 0
defects (a:as) (b:bs)
    | a == b = defects as bs
    | otherwise = 1 + defects as bs

-----

puzzlesP :: Parser [[String]]
puzzlesP = sepEndBy1 puzzleP newline <* eof

puzzleP :: Parser [String]
puzzleP = many1 lineP

lineP :: Parser String
lineP = many1 (oneOf ".#") <* newline