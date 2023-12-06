{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Main where

import AOC
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import qualified Text.Parsec.Char as P
import qualified Text.Parsec.Combinator as P
import qualified Data.Set as S
import qualified Data.Array as A
import Data.Binary.Get (label)
import Debug.Trace (traceShow)

main :: IO ()
main = AOC.runStringSolution (AOC.Solution
  { dayNum=4
  , solver=
    [ AOC.ParseSolver cardsP solve1
    , AOC.ParseSolver cardsP solve2
    ]
  , testWants=["13","30"]
  })

solve1 :: [Card] -> String
solve1 = show . sum . map scoreCard


winningNums :: Card -> Int
winningNums (Card i win hav) = S.size (S.fromList win `S.intersection` S.fromList hav)

scoreCard :: Card -> Int
scoreCard c =
   case winningNums c of
    0 -> 0
    x -> 2 ^ (x-1)

solve2 :: [Card] -> String
solve2 cs =
  let
    scores = map winningNums cs

    accumScores :: (Int, [Int]) -> Int -> (Int, [Int])
    accumScores (tot, s:ss) n =
      let
        nn = s * n
        addS = replicate n s
        newSS = zipWith (+) ss addS ++ drop n ss
      in (tot + s, newSS)
   in
    show . fst $ foldl accumScores (0, [1,1..]) scores

data Card = Card { cardNo :: Int, winning :: [Int], have :: [Int] }
  deriving (Show)

cardsP :: P.Parser [Card]
cardsP = P.sepEndBy1 cardP P.newline

cardP :: P.Parser Card
cardP = do
  P.string "Card"
  P.many1 (P.char ' ')
  num <- numberP
  P.string ":"
  win <- numbersP
  P.string " |"
  hav <- numbersP
  pure $ Card num win hav

numberP :: P.Parser Int
numberP = read <$> P.many1 P.digit

numbersP :: P.Parser [Int]
numbersP = P.many1 (P.try (P.many1 (P.char ' ') *> numberP))
