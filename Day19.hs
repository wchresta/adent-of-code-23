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
import Control.Applicative ((<|>))
import Data.Functor (($>))


main :: IO ()
main = do
    AOC.runSolverWithTests (AOC.SolutionWithTests
        { dayNums=19
        , solvers=
            [ AOC.parseSolver systemP solve1 [("test1", "19114")]
            , AOC.parseSolver systemP solve2 [("test1", "167409079868000")]
            ]
        })

newtype Workflows = Workflows { wMap :: M.Map String [Rule] }
  deriving (Show, Eq, Ord)

data Rule
  = Test Char Ordering Int Result
  | Direct Result
  deriving (Show, Eq, Ord)

data Result
  = Accepted
  | Rejected
  | Goto String
  deriving (Show, Eq, Ord)

newtype Rating = Rating { rMap :: M.Map Char Int }
  deriving (Show, Eq, Ord)

----

solve1 :: (Workflows, [Rating]) -> String
solve1 (w, rs) 
  = show 
  . sum
  . map snd
  . filter ((==Accepted) . fst) 
  . map (\rat -> (runWorkflow w "in" rat, sum . M.elems . rMap $ rat)) 
  $ rs

solve2 :: (Workflows, a) -> String
solve2 (w, _) 
  = show
  . sum
  . map (product . map (\(x,y) -> y-x+1) . M.elems)
  . runInterval w "in"
  . M.fromList 
  $ map (,(1,4000)) "xmas"

runInterval :: Workflows -> String -> M.Map Char (Int,Int) -> [M.Map Char (Int,Int)]
runInterval w name intervalMap =
    evalRules intervalMap rules
  where
    Just rules = M.lookup name (wMap w)

    evalResult ints Accepted = [ints]
    evalResult ints Rejected = []
    evalResult ints (Goto newName) = runInterval w newName ints

    evalRules ints [Direct res] = evalResult ints res
    evalRules ints (Test v ord n res:ts) =
      let Just (x,y) = M.lookup v ints
          with (a,b) f s | a>b = []
          with (a,b) f s = f (M.insert v (a,b) ints) s
       in case ord of
           ---       x......y  
           --- n:  2    3     1
           LT | y < n -> evalResult ints res
           LT | n <= x -> evalRules ints ts
           LT -> with (x,n-1) evalResult res ++ with (n,y) evalRules ts

           GT | n < x -> evalResult ints res
           GT | y <= n -> evalRules ints ts
           GT -> with (x,n) evalRules ts ++ with (n+1,y) evalResult res


runWorkflow :: Workflows -> String -> Rating -> Result
runWorkflow w name r =
  case evalRules rules of
    Goto newName -> runWorkflow w newName r    
    endResult    -> endResult
  where
    rules = fromJust $ M.lookup name (wMap w)

    evalRules [Direct res] = res
    evalRules (Test x ord y res:ts) =
      case M.lookup x (rMap r) of
        Nothing -> evalRules ts
        Just a  -> if compare a y == ord then res else evalRules ts

----

systemP :: Parser (Workflows, [Rating])
systemP = (,) <$> workflowsP <* newline <*> many1 ratingP <* eof

workflowsP :: Parser Workflows
workflowsP = Workflows . M.fromList <$> many1 ((,) <$> many1 alphaNum <*> rulesP <* newline)

rulesP :: Parser [Rule]
rulesP = between (char '{') (char '}') $ sepBy1 ruleP (char ',')

ruleP :: Parser Rule
ruleP = (char 'A' $> Direct Accepted)
      <|> (char 'R' $> Direct Rejected)
      <|> testP

testP :: Parser Rule
testP = do
  name <- many1 alphaNum
  if length name == 1
    then Test (head name) <$> (asOrdering <$> oneOf "<>") <*> (read <$> many1 digit) <* char ':' <*> resultP
    else pure $ Direct (Goto name)
  where
    asOrdering '>' = GT
    asOrdering '<' = LT

resultP :: Parser Result
resultP = char 'A' $> Accepted <|> char 'R' $> Rejected <|> (Goto <$> many1 alphaNum)

ratingP :: Parser Rating
ratingP = Rating . M.fromList <$> 
            between (char '{') (char '}') 
              (sepBy1 ratingEntryP (char ',') )
            <* newline

ratingEntryP :: Parser (Char, Int)
ratingEntryP = (,) <$> oneOf "xmas" <*> (char '=' *> (read <$> many1 digit))