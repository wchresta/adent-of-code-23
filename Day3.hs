{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Main where

import AOC
import qualified Data.Text.Read as T
import qualified Data.Text as T
import Data.Char
import Debug.Trace
import Data.Bifunctor (second)
import qualified Data.Map.Strict as M

main :: IO ()
main = AOC.runStringSolution (AOC.Solution
  { dayNum=3
  , solver=
    [ AOC.BlockSolver process1
    , AOC.BlockSolver process2
    ]
  , testWants=["4361","467835"]
  })

process1 :: String -> String
process1 block =
  let
    grid = lines block
    (maxI, maxJ) = (length grid-1, (length . head $ grid) - 1)
    numPos = extNumBlock grid

    checkValid (a,i,n,m) =
      if any checkOne
            ([ (i, n-1)
            , (i, m+1)
            ] ++ map (i-1,) [n-1..m+1]
            ++ map (i+1,) [n-1..m+1])
        then a
        else 0
    checkOne (i,j)
        | i >= 0 && j >= 0 && i <= maxI && j <= maxJ =
          let d = ((grid !! i) !! j) in not (isDigit d) && d /= '.'
        | otherwise = False
  in
    show . sum . map checkValid $ numPos

process2 :: String -> String
process2 block =
  let
    grid = lines block
    (maxI, maxJ) = (length grid-1, (length . head $ grid) - 1)
    numPos = extNumBlock grid

    findGears (a,i,n,m) =
      concatMap (checkGear a)
            ([ (i, n-1)
            , (i, m+1)
            ] ++ map (i-1,) [n-1..m+1]
            ++ map (i+1,) [n-1..m+1])

    checkGear :: Int -> (Int, Int) -> [((Int,Int),(Int,Int))]
    checkGear a (i,j)
        | i >= 0 && j >= 0 && i <= maxI && j <= maxJ
           && grid !! i !! j == '*' = [((i,j),(1,a))]
        | otherwise = []
    
    gearsTouched :: [((Int,Int),(Int,Int))]
    gearsTouched = concatMap findGears numPos
    
    collectGears :: [(Int,Int)]
    collectGears = M.elems . M.fromListWith (\(i,a) (j,b) -> (i+j, a*b)) $ gearsTouched
  in
    show . sum . map snd . filter ((>1) . fst) $ collectGears

extNumBlock :: [String] -> [(Int, Int, Int, Int)]
extNumBlock
  = concatMap (\(i,s) -> map (\(a,n,m) -> (a,i,n,m)) (extNumLine s))
  . zip [0..]

extNumLine :: String -> [(Int, Int, Int)]
extNumLine = f 0 []
  where
    f _ [] [] = []
    f b m [] = [(read m, b-length m, b-1)]

    f b m  (d:ss) | isDigit d = f (b+1) (m ++ [d]) ss
    f b [] (d:ss) = f (b+1) [] ss
    f b m  (d:ss) = (read m, b-length m, b-1):f (b+1) [] ss
