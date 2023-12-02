module AOC where

import qualified System.IO
import qualified Data.Text as T

type Solver a = [a]

data LineSolution a b = SolveByLines
 { dayNum :: Int
 , solver :: Solver (a -> b, [b] -> a)
 , testWants :: [a]
 }

runSolve :: LineSolution String a -> IO ()
runSolve s = do
  let dayDir = "day" ++ show (dayNum s)
  let (fstSolve, fstCollect) = head $ solver s
  let runTest (want,(solver, collecter),testFile) = do
        got <- processLines solver collecter testFile
        test want got
  let testFiles  = map (((dayDir ++ "/test") ++) .  show) [1..]

  mapM_ runTest (zip3 (testWants s) (solver s) testFiles)

  results <- mapM (\(solve,collect) -> processLines solve collect (dayDir ++ "/input")) (solver s)
  putStrLn $ unlines results

runTextSolve :: LineSolution T.Text a -> IO ()
runTextSolve s = runSolve $ SolveByLines {
  dayNum = dayNum s,
  solver = map (\(ab, ba) -> (ab . T.pack, T.unpack . ba)) $ solver s,
  testWants = map T.unpack $ testWants s
}

readLines :: FilePath -> IO [String]
readLines path = do
  content <- readFile path
  return $ lines content

processLines :: (String -> a) -> ([a] -> String) -> FilePath -> IO String
processLines process collect file = do
  lines <- readLines file
  pure $ collect . map process $ lines

test :: String -> String -> IO ()
test want got = do
  putStr "Test "
  if want == got
    then putStrLn $ "succeeded:  got " ++ got
    else putStrLn $ "**failed**: got " ++ got ++ " but wanted " ++ want

