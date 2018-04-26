module Level22.Problem549
    --problem
  (
  ) where

import Data.List
import Data.Numbers.Primes

-- import Problem
-- problem :: Problem Integer
-- problem = Problem {ind = 549, name = "Divisibility of factorials", solution = 0}
capitalS n = sum $ map kempner [2 .. n]

-- -- So it turns out that this is called the Kempner function or the Smarandache
-- -- function
s n = fst $ head $ filter ((== 0) . (`mod` n) . snd) factorials

-- kempner n
--   | n == 1 = 1
--   | isPrime n = n
--   | otherwise =
--     case group $ primeFactors n of
--       [x] ->
--         let p = head x
--             a = length x
--         in if a <= p
--              then a * p
--              else s n
--       l -> maximum $ map (\l' -> kempner $ head l' ^ length l') l
-- -- s n = sum $ last $ group $ primeFactors n
factorials = zip [1 ..] $ scanl (*) 1 [2 ..]

-- factorial n = product [2 .. n]
kempner n = helper $ primeFactors n
  where
    helper l =
      case l of
        [] -> 0
        p:xs ->
          let e = (1 +) $ length $ takeWhile (== p) xs
              rest = dropWhile (== p) xs
          in max (sForPrimePower p e) $ helper rest

sForPrimePower p e = helper p e 0
  where
    helper p e k =
      let k' = k + p
          e' = e - p - 1
      in if e > p
           then let loop t
                      | t `rem` p == 0 = loop (t `div` p) - 1
                      | otherwise = 0
                in helper p (e' - (loop $ k' `div` p)) k'
           else (k + max 0 e) * p

compareKS b = filter (\(_, (x, y)) -> x /= y) $ zip [1 ..] $zip kemp ss
  where
    kemp = map kempner [1 .. b]
    ss = map s [1 .. b]
