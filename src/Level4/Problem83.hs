module Level4.Problem83
  ( problem
  ) where

import Data.Array
import Data.List
import qualified Data.Map as M
import Level4.Problem83Matrix
import Problem

problem :: Problem Integer
problem =
  Problem
  { ind = 83
  , name = "Path sum: four ways"
  , solution = bfs $ listMatrixToArr matrix
  }

-- minPathSum ::
--      (Num a2, Num a, Num a1, Ord a2, Ix a1, Ix a) => Array a1 (Array a a2) -> a2
-- minPathSum arr = helper M.empty (0, 0) M.! (0, 0)
--   where
--     helper memo c
--       | M.member c memo = memo
--       | atCorner arr c = M.insert c (fromMaybe (-10 ^ 200) $ arr !!!? c) memo
--       | otherwise =
--         case arr !!!? c of
--           Nothing -> M.insert c (10 ^ 200) memo
--           Just v ->
--             M.insert c (v + (minimum $ map (nextM M.!) $ allDirections c)) nextM
--             where nextM =
--                     foldl helper (M.insert c (10 ^ 200) memo) $ allDirections c
bfs :: Array Int (Array Int Integer) -> Integer
bfs arr = helper [((0, 0), arr !!? (0, 0))] M.empty
  where
    helper [] _ = 10 ^ 200
    helper ((c, v):q) visited
      | atCorner arr c = v
      | otherwise =
        helper
        -- Using some sort of sorted data structure would be a lot more
        -- efficient than resorting each iteration, but this still yields a result in a reasonable amount of time
          (sortBy
             (\(_, x) (_, x') -> compare x x')
             (q ++
              (map (\c' -> (c', v + (arr !!? c'))) $
               filter (not . (flip M.member) visited) $
               filter (\(x, y) -> isInbounds arr x && isInbounds (arr ! x) y) $
               allDirections c)))
          (M.insert c True visited)

atCorner :: (Ix i, Eq a) => Array i (Array a e) -> (i, a) -> Bool
atCorner mtrx (x, y) = x == snd (bounds mtrx) && y == snd (bounds (mtrx ! x))

allDirections :: (Num a, Num a1) => (a1, a) -> [(a1, a)]
allDirections (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

(!!?) :: Ix i => Array i (Array i e) -> (i, i) -> e
(!!?) arr (x, y) = (arr ! x) ! y

isInbounds :: Ord a => Array a e -> a -> Bool
isInbounds arr x = x >= minX && x <= maxX
  where
    (minX, maxX) = bounds arr

listMatrixToArr :: [[e]] -> Array Int (Array Int e)
listMatrixToArr = f . map f
  where
    f l = listArray (0, length l - 1) l
