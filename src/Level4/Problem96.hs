module Level4.Problem96
    --problem
  (
  ) where

import Data.Char
import Data.List
import Data.Matrix
import qualified Data.Vector as V

-- import Level4.Problem96Puzzles
-- import Problem
-- problem :: Problem Integer
-- problem = Problem 96 "Su Doku" (sum $ map solve $ map mPuzzle puzzles)
mPuzzle = fromLists . initializePuzzle

solve m
  | isSolved m =
    read $
    map (intToDigit . fromIntegral) $ concat $ take 3 $ V.toList $ getRow 1 m
  | doIteration m == m = error $ "stuck:\n" ++ show m
  | otherwise = solve $ doIteration m

isSolved = V.all ((== 1) . length) . V.take 3 . getRow 1

doIteration p = clearColumns $ clearRows $ clearBlocks p

clearColumns p = foldl clearColumn p [1 .. 9]

clearRows p = foldl clearRow p [1 .. 9]

clearBlocks p = foldl clearBlock p [(x, y) | x <- [0 .. 2], y <- [0 .. 2]]

clearColumn p x = mapCol (\i _ -> cleared !! (i - 1)) x p
  where
    cleared = clearLine $ V.toList $ getCol x p

clearRow p y = mapRow (\i _ -> cleared !! (i - 1)) y p
  where
    cleared = clearLine $ V.toList $ getRow y p

clearBlock p (bx, by) =
  foldl
    (\m (x, y) -> setElem (cleared !! coordToInd x y) (x, y) m)
    p
    blockCoords
  where
    cleared =
      clearLine $
      toList $ submatrix (1 + by * 3) (by * 3 + 3) (1 + bx * 3) (bx * 3 + 3) p
    coordToInd x y = (x - (1 + bx * 3)) * 3 + (y - (1 + by * 3))
    blockCoords =
      [ (x, y)
      | x <- [1 + bx * 3 .. bx * 3 + 3]
      , y <- [1 + by * 3 .. by * 3 + 3]
      ]

submat' bx by = submatrix (1 + by * 3) (by * 3 + 3) (1 + bx * 3) (bx * 3 + 3)

filterIfNot1 f e =
  if length e > 1
    then filter f e
    else e

checkDuplicates l = length filtered == length unfiltered
  where
    unfiltered = map head $ filter ((== 1) . length) l
    filtered = nub unfiltered

checkCol p x = checkDuplicates $ V.toList $ getRow x p

checkRow p y = checkDuplicates $ V.toList $ getCol y p

checkBlock p (bx, by) =
  checkDuplicates $
  toList $ submatrix (1 + by * 3) (by * 3 + 3) (1 + bx * 3) (bx * 3 + 3) p

checkPuzzle p =
  all (checkCol p) [1 .. 9] &&
  all (checkRow p) [1 .. 9] &&
  all (checkBlock p) [(x, y) | x <- [0 .. 2], y <- [0 .. 2]]

-- bruteForce p = 
initializePuzzle =
  map
    (map
       (\x ->
          if x == 0
            then [1 .. 9]
            else [x]))

testPuzzle =
  [ [2, 0, 0, 0, 8, 0, 3, 0, 0]
  , [0, 6, 0, 0, 7, 0, 0, 8, 4]
  , [0, 3, 0, 5, 0, 0, 2, 0, 9]
  , [0, 0, 0, 1, 0, 5, 4, 0, 8]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [4, 0, 2, 7, 0, 6, 0, 0, 0]
  , [3, 0, 1, 0, 0, 7, 0, 4, 0]
  , [7, 2, 0, 0, 4, 0, 0, 6, 0]
  , [0, 0, 4, 0, 1, 0, 0, 0, 3]
  ]

testPuzzle1 =
  [ [0, 0, 3, 0, 2, 0, 6, 0, 0]
  , [9, 0, 0, 3, 0, 5, 0, 0, 1]
  , [0, 0, 1, 8, 0, 6, 4, 0, 0]
  , [0, 0, 8, 1, 0, 2, 9, 0, 0]
  , [7, 0, 0, 0, 0, 0, 0, 0, 8]
  , [0, 0, 6, 7, 0, 8, 2, 0, 0]
  , [0, 0, 2, 6, 0, 9, 5, 0, 0]
  , [8, 0, 0, 2, 0, 3, 0, 0, 9]
  , [0, 0, 5, 0, 1, 0, 3, 0, 0]
  ]

clearLine :: Ord b => [[b]] -> [[b]]
clearLine = helper 1
  where
    helper n l =
      if n > 9
        then l
        else helper (n + 1) $ map removeDefinitesFrom l
      where
        removeDefinitesFrom cs =
          filter
            (\x ->
               notElem x $ concatMap (\d -> [d | shouldFilter cs d]) definites)
            cs
        shouldFilter cs x = not (length cs <= n && elem x cs)
        definites =
          concat $
          concat $
          filter ((==) n . length) $ group $ sort $ filter ((==) n . length) l
