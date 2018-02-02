module Level1.Problem15
  ( problem
  ) where

import Problem

problem :: Problem Integer
problem = Problem {ind = 15, name = "Lattice paths", solution = solver 20}

-- Some explanation:
-- A path that traverses a lattice of N*N size is a path of length 2N in which
-- we go down as many times as we go right.
-- So the problem reduces to calculating N choose 2N or the binomanial
-- coefficient of N and 2N.
solver :: Integer -> Integer
solver n = truncate $ foldl (*) 1 $ map f [1 .. toRational n]
  where
    f i = ((2 * (fromIntegral n) + 1 - i) / i) :: Rational
