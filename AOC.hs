module AOC where

import qualified System.IO
import qualified Data.Text as T

data Solver a b = LineSolver { processLine :: a -> b, collectLines :: [b] -> a }

runStringSolver :: Solver String b -> String -> String
runStringSolver (LineSolver l c) = c . map l . lines

toStringSolver :: Solver T.Text b -> Solver String b
toStringSolver (LineSolver l c) = LineSolver (l . T.pack) (T.unpack . c)

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

  mapM_ runTest (zip3 (testWants s) (solver s) testFiles)

  inp <- readFile $ dayDir ++ "/input"
  results <- mapM (pure . (`runStringSolver` inp)) (solver s)
  putStrLn $ unlines results

runTextSolve :: Solution T.Text a -> IO ()
runTextSolve s = runStringSolution $ Solution
 { dayNum = dayNum s
 , solver = map toStringSolver (solver s)
 , testWants = map T.unpack $ testWants s
 }


test :: String -> String -> IO ()
test want got = do
  putStr "Test "
  if want == got
    then putStrLn $ "succeeded:  got " ++ got
    else putStrLn $ "**failed**: got " ++ got ++ " but wanted " ++ want

