{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Main where

import AOC
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Debug.Trace (traceShow)

main :: IO ()
main = AOC.runStringSolution (AOC.Solution
  { dayNum=5
  , solver=
    [ AOC.ParseSolver almanacP (show . minimum . runAlmanac1)
    , AOC.ParseSolver almanacP (show . minimum . runAlmanac2)
    ]
  , testWants=["35","46"]
  })

data Recipe = Recipe { toR :: String, fromR :: String, entriesP :: [(Int, Int, Int)] } deriving Show
data Almanac = Almanac { seeds :: [Int], recipes :: [Recipe] } deriving Show

chain :: [a -> a] -> a -> a
chain fs a = foldl (\a f -> f a) a fs

chainProb :: [a -> [a]] -> a -> [a]
chainProb [] a = [a]
chainProb (f:fs) a = concatMap (chainProb fs) (f a)

runRecipe :: [(Int,Int,Int)] -> Int -> Int
runRecipe [] x = x
runRecipe ((t,f,d):rs) x
  | f <= x && x <= f + d = t + (x - f)
  | otherwise = runRecipe rs x

runAlmanac1 :: Almanac -> [Int]
runAlmanac1 a = do
  x <- seeds a
  let fs = map (runRecipe . entriesP) (recipes a)
  pure $ chain fs x

runRecipeOnRange :: [(Int, Int, Int)] -> (Int,Int) -> [(Int,Int)]
runRecipeOnRange _ (a,b) | a >= b = traceShow ("ERR",a,b) $ error "BREAK"  -- Empty interval
runRecipeOnRange [] x = [x]
runRecipeOnRange rs@(c@(t,f,d):rss) (a,b)
  | f <= a && b <= f + d = [(t - f + a, t - f + b)] -- [a,b] fully contained
  | b < f || f + d < a = -- [a,b] fully outside
        runRecipeOnRange rss (a,b)
  | a < f && f + d < b = -- [a,b] fully envelops source interval
        runRecipeOnRange rs (a,f-1) ++ runRecipeOnRange rs (f,f+d) ++ runRecipeOnRange rs (f+d+1,b) 
  | f <= a && a <= f + d = -- [a,b] partially contained with a in, b out
        runRecipeOnRange rs (a,f+d) ++ runRecipeOnRange rs (f+d+1,b)
  | f <= b && b <= f + d = -- [a,b] partially contained with a out, b in
        runRecipeOnRange rs (a,f-1) ++ runRecipeOnRange rs (f,b)
runRecipeOnRange r x = error $ show ("ERROR",head r,x)

runAlmanac2 :: Almanac -> [Int]
runAlmanac2 a =
  let
     fs = map (runRecipeOnRange . entriesP) (recipes a)
     resultRanges = concatMap (chainProb fs) $ toRanges (seeds a)
  in map fst resultRanges

toRanges :: [Int] -> [(Int,Int)]
toRanges [] = []
toRanges (a:r:rs) = (a,a+r):toRanges rs

almanacP = do
  string "seeds:"
  seeds <- numbersP
  newline
  newline
  rs <- sepBy1 recipeP (try newline)
  eof
  pure $ Almanac seeds rs

recipeP :: Parser Recipe
recipeP = do
  from <- wordP
  string "-to-"
  to <- wordP
  string " map:"
  newline
  rs <- sepEndBy1 (try rEntryP) newline
  pure $ Recipe from to rs

rEntryP :: Parser (Int, Int, Int)
rEntryP = do
  (,,) <$> numberP <*> (string " " *> numberP) <*> (string " " *> numberP)

numbersP = many1 (try (many1 (char ' ')) *> numberP)

numberP :: Parser Int
numberP = read <$> many1 digit

wordP :: Parser String
wordP = many1 alphaNum
