{-# LANGUAGE RankNTypes #-}

module Problem.Level1.Problem5
  ( problem
  ) where

import Problem.Problem

problem :: Problem Integer
problem = Problem {ind = 5, name = "Smallest multiple", solution = solver 20}

solver :: Integer -> Integer
solver bound =
  foldl (*) 1 $ foldl mustHaveAtLeast [] $ map factorize [1 .. bound]

mustHaveAtLeast :: Eq a => [a] -> [a] -> [a]
mustHaveAtLeast l [] = l
mustHaveAtLeast [] b = b
mustHaveAtLeast l (x:xs) =
  with ++
  mustHaveAtLeast without xs ++
  if any (== x) with
    then []
    else [x]
  where
    (with, without) = seperate (/= x) l

seperate :: forall a. (a -> Bool) -> [a] -> ([a], [a])
seperate f l =
  if length p2 /= 0
    then (p1 ++ [head p2], tail p2)
    else (p1, p2)
  where
    p1 = takeWhile f l
    p2 = dropWhile f l

factorize :: Integral t => t -> [t]
factorize x
  | x == 2 = [2]
  | x < 2 = []
  | otherwise =
    if length divs > 0
      then smallestDiv : factorize (div x smallestDiv)
      else [x]
  where
    divs = filter ((==) 0 . rem x) [2 .. x]
    smallestDiv = head divs
