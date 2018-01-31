module Problem.Level1.Problem19
  ( problem
  ) where

import Problem.Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 19
  , name = "Counting Sundays"
  , solution = fromIntegral $ sundaysOnFirstIn [1901 .. 2000]
  }

sundaysOnFirstIn :: [Int] -> Int
sundaysOnFirstIn =
  length .
  filter (== 6) . concatMap (\y -> map (\m -> dayOfTheWeek y m 1) [1 .. 12])

dayOfTheWeek :: Int -> Int -> Int -> Int
dayOfTheWeek year month day =
  mod
    (d + truncate (2.6 * (fromIntegral m) - 0.2) + y + (quot y 4) + (quot c 4) -
     2 * c)
    7
  where
    d = day
    m = (mod (month - 3) 12) + 1
    y' =
      if m > 10
        then year - 1
        else year
    y = mod y' 100
    c = quot y' 100
