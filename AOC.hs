module AOC where

import qualified System.IO
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P

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
  let dayDir = "day" ++ show (dayNum s)
  let runTest (want,slvr,testFile) = do
        inp <- readFile testFile
        let got = runStringSolver slvr inp
        test want got
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

