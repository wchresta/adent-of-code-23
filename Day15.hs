{-# LANGUAGE LambdaCase #-}
module Main where

import AOC
import Text.Parsec hiding ((<|>))
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Debug.Trace (trace, traceShow, traceShowId)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Bifunctor
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
import Data.Tuple (swap)
import Data.Char (ord)
import Control.DeepSeq

main :: IO ()
main = do
    AOC.runStringSolution (AOC.Solution
        { dayNum=15
        , solver=
            [ AOC.ParseSolver entriesP solve1
            , AOC.ParseSolver entriesP solve2
            ]
        , testWants=["1320", "145"]
        })

data Instr 
  = Ass String Int
  | Dec String

instance Show Instr where
  show (Ass s i) = s ++ "=" ++ show i
  show (Dec s) = s ++ "-"


solve1 :: [Instr] -> String
solve1 = show . sum . map (hash . show)

solve2 :: [Instr] -> String
solve2 = show . score . foldl run emptyBoxes . withHash
  where
    emptyBoxes :: M.Map Int [(String, Int)]
    emptyBoxes = M.fromList [(h,[]) | h <- [0..255]]

    withHash :: [Instr] -> [(Int, Instr)]
    withHash = map (\x -> (labelHash x, x))

    run :: M.Map Int [(String,Int)] -> (Int, Instr) -> M.Map Int [(String, Int)]
    run s (h,Ass l i) = M.adjust (upsert (l,i)) h s
    run s (h,Dec l) = M.adjust (delete l) h s

    upsert :: (String, Int) -> [(String, Int)] -> [(String, Int)]
    upsert li [] = [li]
    upsert (l,i) ((k,j):ls)
      | l == k = (l,i):ls
      | otherwise = (k,j):upsert (l,i) ls
    
    delete :: String -> [(String, Int)] -> [(String, Int)]
    delete l [] = []
    delete l ((x,i):ls)
      | x == l = ls
      | otherwise = (x,i):delete l ls

    score :: M.Map Int [(String, Int)] -> Int
    score = sum . map s . M.assocs
      where
        s (b,as) = (b+1) * sum (zipWith (*) [1..] (map snd as))


hash :: String -> Int
hash = foldl f 0 . map ord
  where
    f t n = 17 * (t+n) `mod` 256

labelHash :: Instr -> Int
labelHash (Ass s _) = hash s
labelHash (Dec s) = hash s

entriesP :: Parser [Instr]
entriesP = sepBy1 instrP (char ',')

instrP :: Parser Instr
instrP = do
  var <- many1 letter
  op <- oneOf "-="
  case op of
    '-' -> pure $ Dec var
    '=' -> Ass var <$> read <$> many1 digit
