module Level5.Problem100
  ( problem
  ) where

import Data.List
import Data.Maybe

import Problem

problem :: Problem Integer
problem =
  Problem
    100
    "Arranged probability"
    (fst $ head $ dropWhile ((< 10 ^ 12) . snd) solutions')

solvePolynomial :: Integral t => t -> t -> t -> Maybe t
solvePolynomial a b c = do
  d <- safeSqrt discriminant
  safeDiv
    (if d > b
       then d - b
       else -d - b)
    (2 * a)
  where
    discriminant = b ^ 2 - 4 * a * c

safeSqrt :: Integral a => a -> Maybe a
safeSqrt x
  | x < 0 = Nothing
  | root ^ 2 == x = Just root
  | otherwise = Nothing
  where
    root = truncate $ sqrt $ fromIntegral x

safeDiv :: Integral a => a -> a -> Maybe a
safeDiv a b =
  if a `rem` b == 0
    then Just $ a `div` b
    else Nothing

findFirstCombination :: Integral a => a -> Maybe a
findFirstCombination bound = find (isJust . blueBalls) [bound ..]

blueBalls :: Integral t => t -> Maybe t
blueBalls total = solvePolynomial 2 (-2) (total - (total ^ 2))

-- (x / y) * (x-1/(y-1)) = 1/2
-- 2*x^2 - 2*x + y - y^2 = 0
-- disc = 4(2y^2 - 2y + 1)
-- s1 = (1 + sqrt(2y^2 - 2y + 1))/2
-- s2 = (1 - sqrt(2y^2 - 2y + 1))/2
-- 2y^2 - 2y  > -1
-- 2y - 2y^2 < 1
-- y < 1/2 + y^2
--
-- Integer
-- integer                        -> (1 + sqrt(2y^2 - 2y + 1)) / 2
-- even                           -> 1 + sqrt(2y^2 - 2y + 1)
-- odd                            -> sqrt(2y^2 - 2y + 1)
-- odd perfect square             -> 2y^2 - 2y + 1
-- (odd perfect square) - 1       -> 2y^2 - 2y
-- ((odd perfect square) - 1) / 2 -> y^2 - y
-- y^2 - y - ps
solutions :: [(Integer, Integer)]
solutions = do
  t <-
    [1,3 ..] >>=
    maybeToList . solvePolynomial 1 (-1) . flip div 2 . (-) 1 . (^ 2)
  b <- maybeToList $ blueBalls t
  return (b, t)

-- After much fiddling about I gave up and googled '"quadratic two integer
-- equation" which lead me here https://www.alpertron.com.ar/QUAD.HTM
-- which I shamefully just copy pasted.
-- I left the original solution for posterity.
solutions' :: [(Integer, Integer)]
solutions' = (15, 21) : map next solutions'
  where
    next (blue, total) = (3 * blue + 2 * total - 2, 4 * blue + 3 * total - 3)
