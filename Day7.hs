{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Main where

import AOC
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Debug.Trace (traceShow)
import Data.Maybe
import Data.List (sortBy, sortOn, group, sort)
import Data.Bifunctor (first)
import qualified Data.Ord

import Test.HUnit hiding (show)

main :: IO ()
main = do
  c <- testHandValues
  if errors c > 0 || failures c > 0
    then putStrLn "** Some unit tests failed **"
    else
      AOC.runStringSolution (AOC.Solution
        { dayNum=7
        , solver=
          [ AOC.ParseSolver bidsP (show . solve)
          , AOC.ParseSolver bidsP (show . solveJoker)
          ]
        , testWants=["6440","5905"]
        })

newtype Hand = Hand { cards :: [Char] }
  deriving (Show, Eq)

newtype JokerHand = JokerHand { innerHand :: Hand }
  deriving (Show, Eq)

data HandType
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfKind
  | FullHouse
  | FourOfKind
  | FiveOfKind
  deriving (Show, Eq, Ord)

instance Ord Hand where
  a `compare` b =
    case handType a `compare` handType b of
      EQ -> map cardValue (cards a) `compare` map cardValue (cards b)
      x  -> x

instance Ord JokerHand where
  a `compare` b =
    case jokerHandType a `compare` jokerHandType b of
      EQ -> map jokerCardValue (cards . innerHand $ a) `compare` map jokerCardValue (cards . innerHand $ b)
      x  -> x

solve hs = 
  let 
    rankedHands = sortOn fst hs
    handsWithRank = zip [1..] rankedHands
    winnings = map (\(rank,(_,bid)) -> rank * bid) handsWithRank
   in sum winnings

solveJoker hs =
  let 
    jokerHands = map (first JokerHand) hs
    rankedHands = sortOn fst jokerHands
    handsWithRank = zip [1..] rankedHands
    winnings = map (\(rank,(_,bid)) -> rank * bid) handsWithRank
   in sum winnings

cardFaces :: [Char]
cardFaces = "23456789TJQKA"

cardValues :: [(Char, Int)]
cardValues = zip cardFaces [2..]

-- Unsafe
cardValue :: Char -> Int
cardValue c =
  case lookup c cardValues of
    Just x -> x
    Nothing -> error $ "Could not lookup " ++ show c ++ ". This should not happen"

jokerCardValue :: Char -> Int
jokerCardValue 'J' = 1
jokerCardValue c = cardValue c

handType :: Hand -> HandType
handType h =
  let
    gs = group (sortedCards h)
    maxSize = maximum . map length $ gs
   in
  case length gs of
    1 -> FiveOfKind
    4 -> OnePair
    5 -> HighCard
    3 -> case maxSize of
      3 -> ThreeOfKind
      2 -> TwoPair
    2 -> case maxSize of
      4 -> FourOfKind
      3 -> FullHouse

jokerHandType :: JokerHand -> HandType
jokerHandType h =
  let
    hands :: [Hand]
    hands = map Hand $ makeHands (cards $ innerHand h)

    makeHands :: [Char] -> [[Char]]
    makeHands [] = [""]
    makeHands ('J':hs) = let rhs = makeHands hs in [ f:rs | f <- cardFaces, f /= 'J', rs <- rhs ]
    makeHands (x:hs) = map (x:) $ makeHands hs
   in
    maximum (map handType hands)

sortedCards :: Hand -> [Char]
sortedCards = sortOn cardValue . cards

--- Tests

testHandValues :: IO Counts
testHandValues = runTestTT . TestList $
  [ TestCase $ assertEqual ("hand value " ++ show a) v (cardValue a)
  | (a,v) <- [('2', 2), ('3', 3), ('T', 10), ('A', 14) ]
  ] ++
  [ TestCase $ assertEqual ("Hand " ++ show a ++ " is " ++ show c ++ " than " ++ show b) c (compare a b)
  | (a,c,b) <- [ (Hand "33332", GT, Hand "2AAAA")
               , (Hand "23333", LT, Hand "2AAAA")
               , (Hand "77888", GT, Hand "77788")
               ]
  ] ++
  [ TestCase $ assertEqual ("Hand " ++ show a ++ " is " ++ show t) t (handType a)
  | (a,t) <- [ (Hand "AAAAA", FiveOfKind)
             , (Hand "23456", HighCard)
             , (Hand "44223", TwoPair)
             , (Hand "44222", FullHouse)
             , (Hand "TT234", OnePair)
             , (Hand "TT2TT", FourOfKind)
             ]
  ] ++
  -- Joker
  [ TestCase $ assertEqual ("JokerHand " ++ show a ++ " is " ++ show t) t (jokerHandType a)
  | (a,t) <- [ (JokerHand (Hand "AAAAA"), FiveOfKind)
             , (JokerHand (Hand "23456"), HighCard)
             , (JokerHand (Hand "44223"), TwoPair)
             , (JokerHand (Hand "44222"), FullHouse)
             , (JokerHand (Hand "TT234"), OnePair)
             , (JokerHand (Hand "TT2TT"), FourOfKind)
             , (JokerHand (Hand "T55J5"), FourOfKind)
             , (JokerHand (Hand "KTJJT"), FourOfKind)
             , (JokerHand (Hand "QQQJA"), FourOfKind)
             ]
  ]

--- Parser

bidsP :: Parser [(Hand, Int)]
bidsP = sepEndBy1 bidP newline <* eof

handP :: Parser Hand
handP = do
  cs <- many1 $ oneOf cardFaces
  if length cs /= 5
    then fail $ "expected 5 cards, but got " ++ show (length cs)
    else pure $ Hand cs

numberP :: Parser Int
numberP = read <$> many1 digit

bidP :: Parser (Hand, Int)
bidP = (,) <$> handP <*> (many1 space *> numberP)
