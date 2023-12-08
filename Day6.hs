module Main where

import AOC
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Debug.Trace (traceShow)

main :: IO ()
main = AOC.runStringSolution (AOC.Solution
  { dayNum=6
  , solver=
    [ AOC.ParseSolver recordsP (show . solve1)
    , AOC.ParseSolver recordsP2 (show . solve1)
    ]
  , testWants=["288","71503"]
  })

type Records = [(Int,Int)] 

{-
  totTime = chargeTime + raceTime
  speed = chargeTime
  distance = raceTime * speed =
           = raceTime * chargeTime =
           = (totTime - chargeTime) * chargeTime
  <=> chargeTime^2 - totTime * chargeTime + distance = 0
  <=> chargeTime_(+/-) = 1/2 * (totTime +/- sqrt{totTime^2 - 4*distance})
                       = 1/2 * (totTime +/- D)
            where D = sqrt{totTime^2 - 4*distance} \elem R

  Ways to win is number of integers in interval
       [chargeTime_-, chargeTime_+]
  Same as number of integers in interval
       [ceil chargeTime_-, floor chargeTime_+]
  Same as number of integers in interval
       floor chargeTime_+ - ceil chargeTime_- + 1

  Ex 1:
    totTime = 7 = chargeTime + raceTime
    distance = 9 = raceTime * speed
    Thus:
     7 = chargeTime + raceTime
     9 = chargeTime * raceTime

    chargeTime_(+/-) = 1/2 * (7 +/- sqrt(49 - 4*9)
                     = (7 +- sqrt{13})/2
                     = 1.69 , 5.30
        =>  floor 5.30 - ceil 1.69 + 1 =
          = 5 - 2 + 1
         -> 4 solutions
  Ex 2:
    totTime = 30
    dist = 200
    chargeTime = 30/2 +/- sqrt{900 - 4*200}/2
               = 15 +/- sqrt{100}/2
               = 15 +- 5
               = 10 , 20
-}
solve1 :: Records -> Int
solve1 = product . map (uncurry numOfWins)

numOfWins :: Int -> Int -> Int
numOfWins t d = floor b - ceiling a + 1 - int a - int b
  where
    (a,b) = winInterval t d
    int x = if snd (properFraction x) == 0.0 then 1 else 0
   

winInterval :: Int -> Int -> (Double, Double)
winInterval t d = (t2 - disc2, t2 + disc2)
  where 
    disc2 = discr t d / 2
    t2 = fromIntegral t / 2
    discr t d = sqrt $ fromIntegral (t^2 - 4*d)

spacesP :: Parser ()
spacesP = many1 (char ' ') *> pure ()

recordsP :: Parser Records
recordsP = do
    string "Time:"
    spacesP
    times <- map read <$> sepBy1 (many1 digit) spacesP
    newline
    string "Distance:"
    spacesP
    dists <- map read <$> sepBy1 (many1 digit) spacesP
    eof
    pure $ zip times dists

recordsP2 :: Parser Records
recordsP2 = do
    string "Time:"
    spacesP
    time <- read . concat <$> sepBy1 (many1 digit) spacesP
    newline
    string "Distance:"
    spacesP
    dist <- read . concat <$> sepBy1 (many1 digit) spacesP
    eof
    pure $ [(time, dist)]