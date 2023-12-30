module Main where

import AOC
import Text.Parsec hiding ((<|>), State(..), getState, lower)
import Text.Parsec.String
import Text.Parsec.Char hiding (lower)
import Text.Parsec.Combinator
import Debug.Trace (trace, traceShow, traceShowId, traceStack)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Linear.V3
import Linear.V2
import Control.Monad (ap)
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Control.Lens ((^.))
import Linear (mult, (^*))
import Data.Maybe (fromMaybe)



main :: IO ()
main = do
    AOC.runSolverWithTests (AOC.SolutionWithTests
        { dayNums=24
        , solvers=
            [ AOC.parseSolver particlesP (solve1 7 27) [("test1", "2")]
            , AOC.parseSolver particlesP (solve1 200000000000000 400000000000000) []
            ]
        })


type V = V3 Double
data Particle = P { _pos :: V, _vel :: V}
  deriving (Eq, Ord)

instance Show Particle where
    show (P pos vel) = '(':showVec pos ++ " @ " ++ showVec vel ++ ")"
      where
        showVec (V3 a b c) = show a ++ "," ++ show b ++ "," ++ show c


solve1 :: Double -> Double -> [Particle] -> String
solve1 from to = show . length
               . filter (uncurry $ doPathsIntersect from to)
               . AOC.pairs

data Intersect a = INowhere | IEverywhere | IAt a
  deriving (Show, Eq, Ord)

doPathsIntersect :: Double -> Double -> Particle -> Particle -> Bool
doPathsIntersect from to a b =
  case particleIntersection a b of
    Just (_, False)     -> False
    Just (V2 x y, True) -> from <= x && x <= to && from <= y && y <= to
    Nothing       -> a == b && travelsThroughBox from to a

travelsThroughBox :: Double -> Double -> Particle -> Bool
travelsThroughBox from to p = 
    any (isIn . fromMaybe (-1,-1) . lineSegmentsIntersect (toSegment p)) 
     [S (V3 from from 0) (V3 from to 0)
     ,S (V3 from from 0) (V3 to from 0)
     ,S (V3 to from 0) (V3 to to 0)
     ,S (V3 from to 0) (V3 to to 0)
     ]
  where
    isIn :: (Double, Double) -> Bool
    isIn (t,u) = t > 0 && 0 <= u && u <= 1

toSegment :: Particle -> Segment V
toSegment (P a v) = S a (a + v)

particleIntersection :: Particle -> Particle -> Maybe (V2 Double, Bool)
particleIntersection a b = lineIntersect (toSegment a) (toSegment b)

data Segment a = S { _from :: a, _to :: a }

lineIntersect :: Segment V -> Segment V -> Maybe (V2 Double, Bool)
lineIntersect a@(S a1 a2) b@(S b1 b2) = do
  (t,u) <- lineSegmentsIntersect a b
  let s = a1 + (a2 - a1) ^* t
  pure (s ^._xy, t > 0 && u > 0)

lineSegmentsIntersect :: Segment V -> Segment V -> Maybe (Double, Double)
lineSegmentsIntersect (S (V3 x1 y1 _) (V3 x2 y2 _)) (S (V3 x3 y3 _) (V3 x4 y4 _)) =
    if denom == 0 then Nothing else Just (partT / denom, partU / denom)
  where
    partT = (x1-x3)*(y3-y4) - (y1-y3)*(x3-x4)
    partU = (x1-x3)*(y1-y2) - (y1-y3)*(x1-x2)
    denom = (x1-x2)*(y3-y4) - (y1-y2)*(x3-x4)


particlesP :: Parser [Particle]
particlesP = many1 particleP <* eof

particleP :: Parser Particle
particleP = P <$> vecP <* string " @ " <*> vecP <* newline

vecP :: Parser V
vecP = V3 <$> intP <* sepP <*> intP <* sepP <*> intP
  where
    intP = ap signP (read <$> many1 digit) <?> "intP"
    signP = (char '-' $> negate) <|> pure id <?> "signP"
    sepP = char ',' <* many1 (char ' ') <?> "sepP"
