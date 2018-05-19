{-# LANGUAGE LambdaCase #-}

module Level4.Problem98
  ( problem
  ) where

import Data.Char
import Data.List
import Data.Map ((!), fromList, member)
import Data.Maybe

import Level4.Problem98Words
import Problem

-- Brute force my dude. Find all anagramic pairs of words then for each pair 
-- try find a mapping that works then select the biggest number in the biggest 
-- mapping.
problem :: Problem Integer
problem =
  Problem
    98
    "Anagramic squares"
    (fromIntegral $ uncurry max $ head $ anagrams >>= uncurry squareMapping)

squaresOfLength :: Integral b => b -> [Integer]
squaresOfLength n =
  takeWhile (< 10 ^ n) $
  dropWhile (< 10 ^ (n - 1)) $
  (^ 2) <$> [intSqrt $ 10 ^ (n - 1) .. intSqrt $ 10 ^ n]

isSquare :: Integer -> Bool
isSquare n = intSqrt n ^ 2 == n

createConverter :: (Ord k, Show p) => [k] -> p -> Maybe ([k] -> Maybe Int)
createConverter word number =
  if isValidAssocs
    then Just f
    else Nothing
  where
    isValidAssocs =
      all
        (\g -> all (== fst (head g)) $ fst <$> g)
        (groupBy (\a b -> snd a == snd b) $
         sortBy (\a b -> snd a `compare` snd b) assocs) &&
      all
        (\g -> all (== snd (head g)) $ snd <$> g)
        (groupBy (\a b -> fst a == fst b) $
         sortBy (\a b -> fst a `compare` fst b) assocs)
    assocs = zip word $ digitToInt <$> show number
    lookup = fromList assocs
    f w =
      if all (`member` lookup) w
        then let number = foldl (\p c -> p * 10 + c) 0 $ map (lookup !) w
              in if length (show number) == length w &&
                    isSquare (fromIntegral number)
                   then Just number
                   else Nothing
        else Nothing

findMappings :: Ord k => [k] -> [[k] -> Maybe Int]
findMappings word =
  (word `createConverter`) <$> candidates >>= \case
    Just f -> [f]
    Nothing -> []
  where
    candidates = reverse $ squaresOfLength $ length word

squareMapping :: Ord k => [k] -> [k] -> [(Int, Int)]
squareMapping w1 w2 =
  findMappings w1 >>= \f ->
    case f w2 of
      Nothing -> []
      Just n -> [(fromMaybe 0 (f w1), n)]

anagrams :: [(String, String)]
anagrams =
  concatMap
    (\case
       [a, b] -> [(a, b)]
       [a, b, c] -> [(a, b), (a, c)]) $
  sortBy (\(a:_) (b:_) -> length b `compare` length a) $
  filter ((> 1) . length) $
  groupBy (\a b -> sort a == sort b) $
  sortBy (\a b -> sort a `compare` sort b) wordList

intSqrt :: Integer -> Integer
intSqrt = truncate . sqrt . fromIntegral
