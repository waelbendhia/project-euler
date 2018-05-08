module Level4.Problem87
  ( problem
  ) where

import Data.Maybe
import Data.Numbers.Primes
import qualified Data.Set as S
import qualified Data.Vector as V

import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 87
  , name = "Prime power triples"
  , solution = toInteger $ countPrimePowers 50000000
  }

countPrimePowers :: Integer -> Int
countPrimePowers n =
  S.size $
  unfoldS
    (\i ->
       if i == V.length pr ^ 3
         then Nothing
         else Just $
              let v = f $ toThruple (V.length pr) i
              in ( if v < n
                     then Just $ f $ toThruple (V.length pr) i
                     else Nothing
                 , if v >= n
                     then skip (V.length pr) i
                     else i + 1))
    S.empty
    0
  where
    f (i1, i2, i3) = (pr V.! i1) ^ 2 + (pr V.! i2) ^ 3 + (pr V.! i3) ^ 4
    b = bound n
    pr = V.fromList $ takeWhile (< b) primes

bound :: Integer -> Integer
bound = truncate . sqrt . fromIntegral . (subtract 2)

toThruple :: Integral t => t -> t -> (t, t, t)
toThruple b i = (x, y, z)
  where
    z = i `mod` b
    y = (i `div` b) `mod` b
    x = i `div` (b ^ 2) `mod` b

skip :: Integral a => a -> a -> a
skip b i = i + (b - i `mod` b)

unfoldS :: Ord a => (t -> Maybe (Maybe a, t)) -> S.Set a -> t -> S.Set a
unfoldS f s i =
  fromMaybe s $
  fmap
    (\(x, nextI) -> unfoldS f (fromMaybe s $ fmap (flip S.insert s) $ x) nextI) $
  f i
