{-# LANGUAGE DeriveAnyClass #-}
module Main where

import AOC
import Text.Parsec hiding ((<|>), State(..))
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Debug.Trace (trace, traceShow, traceShowId)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Numeric (readHex)
import Data.Maybe (fromJust)
import Data.Foldable (foldl')


main :: IO ()
main = do
    AOC.runSolverWithTests (AOC.SolutionWithTests
        { dayNums=18
        , solvers=
            [ AOC.parseSolver (plansP planP1) solve [("test1", "62")]
            , AOC.parseSolver (plansP planP2) solve [("test1", "952408144115")]
            ]
        })

data Dir = U | R | D | L
  deriving (Show, Eq, Ord, Read)

type Colour = String

data Instr = Instr { iDir :: Dir, iLen :: Int }
  deriving (Show, Eq, Ord)

solve :: [Instr] -> String
solve is = show $ enclosedArea is

borderLen :: [Instr] -> Int
borderLen = sum . map iLen

enclosedArea :: [Instr] -> Int
enclosedArea is = border `div` 2 - area + 1 -- Pick's theorem (https://en.wikipedia.org/wiki/Pick%27s_theorem)
  where
    area = shoelace . foldl' makeAbs [(0,0)] . map coordStep $ is
    border = sum $ map iLen is

    coordStep :: Instr -> (Int, Int)
    coordStep (Instr U n) = (n,0)
    coordStep (Instr R n) = (0,n)
    coordStep (Instr D n) = (-n,0)
    coordStep (Instr L n) = (0,-n)

    makeAbs :: [(Int,Int)] -> (Int,Int) -> [(Int,Int)]
    makeAbs xs@((x,y):_) (dx,dy) = (x+dx,y+dy):xs

    -- Shoelace (https://en.wikipedia.org/wiki/Shoelace_formula)
    shoelace cs = (`div`2) . sum $ zipWith (\(x1,y1) (x2,y2) -> (y1+y2)*(x1-x2)) cs (tail cs)

----

plansP :: Parser Instr -> Parser [Instr]
plansP planP = many1 planP <* eof

planP1 :: Parser Instr
planP1 = Instr <$> dirP <* space <*> numberP <* space <* colourP

planP2 :: Parser Instr
planP2 = do
  dirP <* space <* numberP <* space
  (dir, len) <- colourP
  pure $ Instr dir len

dirP :: Parser Dir
dirP = read . pure <$> oneOf "URDL"

numberP :: Parser Int
numberP = read <$> many1 digit

colourP :: Parser (Dir, Int)
colourP = do
  hexString <- string "(#" *> count 5 hexDigit
  dirDigit <- oneOf "0123" <* char ')' <* newline
  let [(len, "")] = readHex hexString
      dir = fromJust $ lookup dirDigit [('0',R),('1',D),('2',L),('3',U)] 
  pure (dir, len)

