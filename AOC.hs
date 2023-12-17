module AOC where

import qualified System.IO
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import qualified Data.Map.Strict as M
import Criterion.Measurement
import Data.List (find)
import Control.DeepSeq (deepseq)

data Solver a b
 = LineSolver { processLine :: a -> b, collectLines :: [b] -> a }
 | BlockSolver { processBlock :: a -> a }
 | ParseSolver { parser :: P.Parser b, processParsed :: b -> a }

runStringSolver :: Solver String b -> String -> String
runStringSolver (LineSolver l c) = c . map l . lines
runStringSolver (BlockSolver p) = p
runStringSolver (ParseSolver p s) =
  \inp -> case P.parse p "input" inp of
            Left err -> error . show $ err
            Right x -> s x

toStringSolver :: Solver T.Text b -> Solver String b
toStringSolver (LineSolver l c) = LineSolver (l . T.pack) (T.unpack . c)
toStringSolver (BlockSolver p) = BlockSolver $ T.unpack . p . T.pack

data Solution a b = Solution
 { dayNum :: Int
 , solver :: [Solver a b]
 , testWants :: [a]
 }

runStringSolution :: Solution String a -> IO ()
runStringSolution s = do
  initializeTime
  let dayDir = "day" ++ show (dayNum s)
  let runTest (want,slvr,testFile) = do
        inp <- readFile testFile
        t1 <- getCPUTime
        let got = runStringSolver slvr inp
        t2 <- deepseq got getCPUTime
        ok <- test want got
        putStrLn $ "    took " ++ show (t2-t1)
        pure ok
  let testFiles  = map (((dayDir ++ "/test") ++) .  show) [1..]

  allRes <- mapM runTest (zip3 (testWants s) (solver s) testFiles)
  if and allRes
    then do
      inp <- readFile $ dayDir ++ "/input"
      results <- mapM (pure . (`runStringSolver` inp)) (solver s)
      putStrLn $ unlines results
    else putStrLn "Some tests failed"


runTextSolution :: Solution T.Text a -> IO ()
runTextSolution s = runStringSolution $ Solution
 { dayNum = dayNum s
 , solver = map toStringSolver (solver s)
 , testWants = map T.unpack $ testWants s
 }


test :: String -> String -> IO Bool
test want got = do
  putStr "Test "
  let ok = want == got
  if ok
    then putStrLn $ "succeeded:  got " ++ got
    else putStrLn $ "**failed**: got " ++ got ++ " but wanted " ++ want
  pure ok

---

toGridWithIgnore :: Eq a => ((Int,Int) -> M.Map (Int,Int) a -> b) -> a -> [[a]] -> b
toGridWithIgnore mk ign as = mk (maxI,maxJ) . M.fromList . filter ((ign/=) . snd) . indexify $ as
  where
    maxI = length as - 1
    maxJ = length (head as) - 1

indexify :: [[a]] -> [((Int, Int), a)]
indexify as = zip [ divMod i n | i <- [0..] ] $ concat as
  where n = length . head $ as

matrixToMap :: [[a]] -> M.Map (Int, Int) a
matrixToMap = M.fromList . indexify

mapToMatrix :: M.Map (Int, Int) a -> (Int, Int) -> a -> [[a]]
mapToMatrix m (maxI, maxJ) a =
  [
    [ M.findWithDefault a (i,j) m | j <- [0..maxJ] ]
    | i <- [0..maxI]
  ]

-- |Non-repeating pairs, excluding (x,x)
pairs :: [a] -> [(a,a)]
pairs [] = []
pairs xs = p (tail xs) xs
  where
    p _ [] = []
    p _ [_] = []
    p [] (_:bs) = p (tail bs) bs
    p (a:as) bs@(b:_) = (a,b) : p as bs

copy :: Int -> a -> [a]
copy 0 _ = []
copy n a = a:copy (n-1) a

cycleN :: Int -> [a] -> [a]
cycleN 0 _ = []
cycleN n as = as ++ cycleN (n-1) as
