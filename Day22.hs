module Main where

import AOC
import Text.Parsec hiding ((<|>), State(..), getState, lower)
import Text.Parsec.String
import Text.Parsec.Char hiding (lower)
import Text.Parsec.Combinator
import Debug.Trace (trace, traceShow, traceShowId)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Numeric (readHex)
import Data.Maybe (fromJust, fromMaybe)
import Data.Foldable (foldl', maximumBy)
import Data.Bifunctor (first)
import Data.List (transpose, sortOn, nub, intercalate)
import Control.Monad.State.Lazy
import Data.Containers.ListUtils (nubOrd)
import Data.Biapplicative (bimap)
import Data.Function ((&))


main :: IO ()
main = do
    AOC.runSolverWithTests (AOC.SolutionWithTests
        { dayNums=22
        , solvers=
            [ AOC.parseSolver bricksP solve1 [("test1", "5")]
            , AOC.parseSolver bricksP solve2 [("test1", "7")]
            ]
        })


data Pos = Pos { x_ :: Int, y_ :: Int, z_ :: Int }
  deriving (Eq, Ord)

instance Show Pos where
  show (Pos x y z) = '(' : show x ++ "," ++ show y ++ "," ++ show z ++ ")"

instance Semigroup Pos where
  Pos a b c <> Pos x y z = Pos (a + x) (b + y) (c + z)

instance Monoid Pos where
  mempty = Pos 0 0 0

data Brick = Brick { bId :: Int, bFrom :: Pos, bTo :: Pos }
  deriving (Eq, Ord)

instance Show Brick where
  show (Brick _ f t) = showPos f ++ "~" ++ showPos t
   where showPos (Pos x y z) = intercalate "," $ map show [x,y,z]

bZ :: Brick -> Int
bZ = z_ . bFrom

bZ2 :: Brick -> Int
bZ2 = z_ . bTo

bXYs :: Brick -> [XY]
bXYs (Brick bid (Pos a b _) (Pos u v _)) = [ (x,y) | x <- [a..u], y <- [b..v] ]

lower :: Int -> Brick -> Brick
lower dz (Brick bid (Pos a b c) (Pos x y z)) = Brick bid (Pos a b (c-dz)) (Pos x y (z-dz))

solve1 :: [Brick] -> String
solve1 bricks
  = show . (1+maxId-) .  S.size
  . S.fromList . concat . filter ((1==) . length)  -- not safe to remove
  . map snd
  . flip evalState M.empty
  . mapM fall
  . sortOn (z_ . bFrom)
  $ bricks
  where maxId = length bricks

type FallMap = M.Map Int (S.Set Int)

solve2 :: [Brick] -> String
solve2 fallingBricks =
    show otherFallCounts
  where
    brickRels = flip evalState M.empty . mapM fall . sortOn (z_ . bFrom) $ fallingBricks

    -- Ordered from lowest to highest
    bricks = map fst brickRels

    dependsOn :: M.Map Int (S.Set Int)
    dependsOn = M.fromList . map (bimap bId S.fromList) $ brickRels

    supports :: M.Map Int (S.Set Int)
    supports = M.fromListWith S.union . map (\(a,b) -> (b,S.singleton a)) . AOC.explodeMap $ dependsOn

    setLookup :: (Ord k, Ord v) => S.Set k -> M.Map k (S.Set v) -> S.Set v
    setLookup ks m = foldl' S.union S.empty . S.map (fromMaybe S.empty . flip M.lookup m) $ ks

    -- remove a brick and return how many bricks are falling
    calcFalling :: Int -> S.Set Int
    calcFalling i = findFalling (S.singleton i) $ S.singleton i
      where
        findFalling deleted toCheck =
           if null toCheck
             then deleted
             else findFalling (S.union deleted directlyFalling) directlyFalling
          where
            potentiallyFalling = setLookup toCheck supports
            directlyFalling = S.map fst 
                            . S.filter (S.null . (`S.difference` deleted) . snd) 
                            . S.map (\s -> (s, fromMaybe S.empty $ M.lookup s dependsOn))
                            $ potentiallyFalling
    
    allFalls :: [S.Set Int]
    allFalls = map calcFalling [1..length bricks]

    otherFallCounts :: Int
    otherFallCounts = sum . map (\s -> S.size s - 1) $ allFalls

type XY = (Int, Int)
type App = State (M.Map XY (Int, Int))

-- Falls a brick and also reports which brick ids it relies on
fall :: Brick -> App (Brick, [Int])
fall brick = do
  let
    xys = bXYs brick
    oldZ = bZ brick
  zs <- gets $ \m -> map (fromMaybe (0,0) . flip M.lookup m) xys
  let newZ = maximum (map snd zs) + 1
      allTouchingIds = nubOrd . map fst . filter ((newZ-1==) . snd) $ zs
      newBrick = lower (oldZ - newZ) brick
  modify' $ M.union (M.fromList . map (,(bId brick, bZ2 newBrick)) $ xys)
  pure (newBrick, allTouchingIds)

bricksP :: Parser [Brick]
bricksP = zipWith (\i (f,t) -> Brick i f t) [1..]
          -- . sortOn (z_ . fst)
          <$> many1 ((,) <$> posP <* char '~' <*> posP <* newline) <* eof
  where posP = Pos <$> compP <* char ',' <*> compP <* char ',' <*> compP
        compP = read <$> many1 digit


