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

main :: IO ()
main = do
  c <- tests
  if errors c > 0 || failures c > 0
    then putStrLn "** Some unit tests failed **"
    else
      AOC.runStringSolution (AOC.Solution
        { dayNum=8
        , solver=
          [ AOC.ParseSolver mapP (show . runMap1)
          , AOC.ParseSolver mapP (show . runMap2)
          ]
        , testWants=["6","6"]
        })

tests :: IO Counts
tests = pure $ Counts 0 0 0 0

type Instruction = (String, (String, String))
data Map = Map String [Instruction]
  deriving (Show)

runMap1 :: Map -> Int
runMap1 (Map path instrs) = runRules 0 "AAA" (cycle path)
  where
    instrMap = M.fromList instrs

    runRules i "ZZZ" _ = i
    runRules i name (x:xs) =
       case x of
        'L' -> runRules (i+1) l xs
        'R' -> runRules (i+1) r xs
      where
        (l, r) = fromJust $ M.lookup name instrMap

-- This solution relies on the fact that the initial
-- steps to get to the periodic behaviour has length
-- 1. This does not generalize, but is easier.
runMap2 m@(Map rule instrs) = 
  let
    instrMap = M.fromList instrs
    starts = [ s | s <- map fst instrs, last s == 'A' ]
    periodLengths = map (findPeriodLength m) starts

    lcms = foldr1 lcm
   in lcms periodLengths
    

findPeriodLength :: Map -> String -> Int
findPeriodLength (Map path instr) root =
  let
    instrMap = M.fromList instr
    pathLen = length path

    walk 'L' pos = fst . fromJust $ M.lookup pos instrMap
    walk 'R' pos = snd . fromJust $ M.lookup pos instrMap

    runOnce [] (trail, pos) = (reverse trail, pos)
    runOnce (w:ws) (trail, pos) = let newPos = walk w pos in runOnce ws (pos:trail, newPos)

    runNTimes 0 r    = []
    runNTimes n r =
      let (trail, newRoot) = runOnce path ([], r)
       in trail ++ runNTimes (n-1) newRoot

    run seen runLength r
      | r `M.member` seen = --traceShow ("Found period", seen, runLength, r) 
                            (r, fromJust $ M.lookup r seen, runLength)
      | otherwise         = 
          let (trailPiece, newRoot) = runOnce path ([], r)
          in
             --traceShow ("Step", seen, runLength, r, trailPiece, newRoot)
             run (M.insert r runLength seen) (runLength + 1) newRoot

    (periodRoot, periodStart, totPeriods) = run M.empty 0 root
  in (totPeriods - periodStart) * pathLen

mapP :: Parser Map
mapP = Map <$> many1 (oneOf "RL") <*> (many1 newline *> sepEndBy1 instructionP newline)

placeP :: Parser String
placeP = many1 $ oneOf (['A'..'Z'] ++ ['1'..'9'])

instructionP :: Parser Instruction
instructionP = do
  name <- placeP
  string " = ("
  left <- placeP
  string ", "
  right <- placeP
  string ")"
  pure (name, (left, right))
  