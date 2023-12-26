{-# LANGUAGE LambdaCase #-}
module Main where

import AOC
import Text.Parsec hiding ((<|>), State(..), getState)
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Debug.Trace (trace, traceShow, traceShowId)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Numeric (readHex)
import Data.Maybe (fromJust)
import Data.Foldable (foldl')
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.Aeson (Value(Bool))
import Control.Monad.State.Strict
import Data.Bifunctor (first)
import Data.List (intercalate)


main :: IO ()
main = do
    AOC.runSolverWithTests (AOC.SolutionWithTests
        { dayNums=20
        , solvers=
            [ AOC.parseSolver modulesP solve1 [("test1", "32000000"), ("test2", "11687500")]
            , AOC.parseSolver modulesP solve2 [
              ("input", "231897990075517")]
            ]
        })


data Module = Module { mType :: MType, mName :: String, mDst :: [String] }
  deriving (Show, Eq, Ord)

data MType
  = Broadcaster
  | FlipFlop
  | Conjunction
  deriving (Show, Eq, Ord)

data MState
  = SBroadcaster
  | SFlipFlop !Bool
  | SConjunction !(M.Map String Bool)
  deriving (Show, Eq, Ord)

type States = M.Map String MState
type Modules = M.Map String Module

initStates :: Modules -> States
initStates ms = M.map makeState ms
  where
    makeState m = case mType m of
      Broadcaster -> SBroadcaster
      FlipFlop -> SFlipFlop False
      Conjunction -> SConjunction (M.fromList [ (dst,False) | dst <- fromJust $ M.lookup (mName m) rDeps])
    rDeps = M.fromListWith (++) [ (dst,[src]) | (src, dsts) <- M.assocs . M.map mDst $ ms, dst <- dsts ]

solve1 :: [Module] -> String
solve1 mList = show . score . evalState (pressButtonN ms 1000) $ ss
  where
    ms = asMap mList
    ss = initStates ms
    score (Score x y) = x * y


type Pulse = (String, String, Bool)

type Machine = State States

send :: Bool -> Module -> [Pulse]
send pulse m = map (mName m,,pulse) . reverse $ mDst m

data Score = Score { lows :: Int, highs :: Int }
  deriving (Show, Eq, Ord)

instance Semigroup Score where
  (Score a b) <> (Score x y) = Score (a+x) (b+y)

instance Monoid Score where
  mempty = Score 0 0

pressButtonN :: Modules -> Int -> Machine Score
pressButtonN ms 1 = pressButton1 ms
pressButtonN ms n = (<>) <$> pressButton1 ms <*> pressButtonN ms (n-1)

pressButton1 :: Modules -> Machine Score
pressButton1 ms = processPulse1 ms ("button", "broadcaster", False)

processPulse1 :: Modules -> Pulse -> Machine Score
processPulse1 ms p = do
  (_,ps) <- processPulseStep ms p
  let
      score (_,_,False) = Score 1 0
      score (_,_,True) = Score 0 1
  case ps of
    [] -> pure (score p)
    _  -> pure (score p) <> mconcat <$> mapM (processPulse1 ms) (reverse ps)  -- Ensure fifo

processPulseStep :: Modules -> Pulse -> Machine ([Pulse], [Pulse])
processPulseStep ms p@(from, to, pulse) = do
  ss <- get
  case M.lookup to ms of
    Nothing -> pure ([p],[])  -- Module not found, is ignored
    Just m ->
      case (mType m, pulse) of
        (Broadcaster, _) -> pure ([],send pulse m)
        (FlipFlop, True) -> pure ([],[])
        (FlipFlop, False) -> do
          let Just (SFlipFlop ffs) = M.lookup (mName m) ss
          modify $ M.insert to (SFlipFlop (not ffs))
          pure ([], send (not ffs) m)
        (Conjunction, _) -> do
          let
            Just (SConjunction mstates) = M.lookup (mName m) ss
            mstates' = M.insert from pulse mstates
            allUp = and (M.elems mstates')
          modify $ M.insert to (SConjunction mstates')
          pure ([], send (not allUp) m)

--- Part 2

solve2 :: [Module] -> String
solve2 mList
  = show
  . foldl1 lcm
  -- . product
  $ evalState (buttonUntil msWithoutPulser rxPulserName $ length rxPulserSources) ss
  where
    -- Assume: There is only one
    -- See day20/graph.png, which was generated from day20/input.dot via `dot -Tpng`
    [rxPulser] = filter (elem "rx" . mDst) mList
    rxPulserName = mName rxPulser
    rxPulserSources = filter (elem rxPulserName . mDst) mList

    ss = initStates ms
    ms = asMap mList
    msWithoutPulser = M.delete rxPulserName ms  -- Remove such that processPulse2 reports the pulses to it

processPulse2 :: Modules -> Pulse -> Machine [Pulse]
processPulse2 ms p = do
  (rest,ps) <- processPulseStep ms p
  case ps of
    [] -> pure rest
    _  -> pure rest <> mconcat <$> mapM (processPulse2 ms) (reverse ps)  -- Ensure fifo

-- Press button until n different sources have sent a HIGH signal to the target
buttonUntil :: Modules -> String -> Int -> Machine (M.Map String Int)
buttonUntil ms toTarget srcCount = f 1 M.empty
  where
    f n res | M.size res == srcCount = pure res
    f n res = do
      rest <- processPulse2 ms ("button", "broadcaster", False)
      let found = map (\(from,_,_) -> from) . filter (\(from,to,pulse) -> pulse && to == toTarget && M.notMember from res) $ rest

      stor <- get

      let deb = if not (null found)
                    then f (n+1) (M.insert (head found) n res)
                    else f (n+1) res

      if 4070 < n && n < 4100
        then trace (show n ++ " " ++ showState stor) deb
        else deb

showState :: States -> String
showState ss = intercalate ";" [ k ++ ":" ++ showState s | (k,s) <- M.assocs ss ]
  where
    showState SBroadcaster = "B"
    showState (SFlipFlop t) = show t
    showState (SConjunction m) = "[" ++ intercalate "," [ k ++ "=" ++ show s | (k,s) <- M.assocs m] ++ "]"

asMap :: [Module] -> Modules
asMap = M.fromList . map (\m -> (mName m, m))

modulesP :: Parser [Module]
modulesP = many1 (moduleP <* newline) <* eof

nameP :: Parser String
nameP = many1 alphaNum

typeP :: Parser MType
typeP = char '%' $> FlipFlop
    <|> char '&' $> Conjunction
    <|> pure Broadcaster

moduleP :: Parser Module
moduleP = Module <$> typeP <*> nameP <* string " -> " <*> sepBy nameP (string ", ")
