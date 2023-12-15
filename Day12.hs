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
import Data.List
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
        { dayNum=12
        , solver=
            [ AOC.ParseSolver springsP solve
            , AOC.ParseSolver springsP $ solve . map (harder 5)
            ]
        , testWants=["21", "525152"]
        })


data SpringRow = SpringRow { row :: String, dmg :: [Int] }
    deriving (Show, Eq)


solve :: [SpringRow] -> String
solve = show . sum . parMap rdeepseq solveOne

solveOne :: SpringRow -> Int
solveOne s = findCount s

harder :: Int -> SpringRow -> SpringRow
harder n (SpringRow r d) =
   SpringRow (intercalate "?" (AOC.copy n r)) (AOC.cycleN n d)

findCount :: SpringRow -> Int
findCount (SpringRow r d) = findCount' r d

findCount' :: String -> [Int] -> Int
findCount' = memoFix2 f
  where
    f g "" [] = 1
    f g "" _ = 0
    f g "." [] = 1
    f g "?" [] = 1
    f g ('#':_) [] = 0
    f g ('?':str) [] = g str []
    f g str@('.':_) ds = g (tail str) ds
    f g str (d:ds) | length str < d = 0
    f g str@('#':_) (d:ds)
         | canEat str d = g (skip $ drop (d+1) str) ds
         | otherwise = 0
    f g str@('?':_) (d:ds)
         | canEat str d = g (skip $ drop (d+1) str) ds + g (skip $ tail str) (d:ds)
         | otherwise    = g (skip $ tail str) (d:ds)
    f g str ds = error $ "No match for" ++ show str ++ " / " ++ show ds

    skip [] = []
    skip ('.':str) = str
    skip str = str

    canEat :: String -> Int -> Bool
    canEat s d
        | length s == d = notElem '.' s
        | length s > d = notElem '.' (take d s) && s !! d /= '#'
        | otherwise = False

springsP :: Parser [SpringRow]
springsP = sepEndBy1 springRowP newline <* eof

springRowP :: Parser SpringRow
springRowP = SpringRow <$> many1 (oneOf "?.#") <*> (space *> dmgP)

dmgP :: Parser [Int]
dmgP = sepBy1 (read <$> many1 digit) (char ',')

