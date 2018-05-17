module Level4.Problem96
  ( problem
  ) where

import Control.Monad.ST
import Data.Array (Array, (!), (//), listArray)
import Data.Array.ST
import Data.List
import Data.Maybe

import Level4.Problem96Puzzles
import Problem

problem :: Problem Integer
problem = Problem 96 "Su Doku" (fromIntegral $ sum $ map runSolve puzzles)

type Sudoku a = STArray a Int Int

readSudoku :: Sudoku e -> Int -> Int -> ST e Int
readSudoku puzzle x y = readArray puzzle (x + 9 * (y - 1))

writeSudoku :: Sudoku e -> Int -> Int -> Int -> ST e ()
writeSudoku puzzle x y = writeArray puzzle (x + 9 * (y - 1))

getBox :: Sudoku e -> Int -> Int -> ST e [Int]
getBox puzzle i j =
  mapM
    (uncurry (readSudoku puzzle))
    [(u, v) | u <- [bI .. bI + 2], v <- [bJ .. bJ + 2]]
  where
    beginBlock x = (3 * ((x - 1) `quot` 3)) + 1
    bI = beginBlock i
    bJ = beginBlock j

available :: Sudoku e -> Int -> Int -> ST e [Int]
available puzzle i j = do
  r <- mapM (flip (readSudoku puzzle) j) [1 .. 9]
  c <- mapM (readSudoku puzzle i) [1 .. 9]
  b <- getBox puzzle i j
  return $ [0 .. 9] \\ foldl union [] [r, c, b]

runSolve :: Foldable t => t [Int] -> Int
runSolve puzzle = 100 * res ! 1 + 10 * res ! 2 + res ! 3
  where
    res =
      runSTArray $ do
        arr <- newListArray (1, 81) $ concat puzzle
        return arr
        solved <- solve arr [(i, j) | i <- [1 .. 9], j <- [1 .. 9]]
        maybe (newArray (1, 3) 0) return solved

solve :: Sudoku e -> [(Int, Int)] -> ST e (Maybe (Sudoku e))
solve puzzle [] = return $ Just puzzle
solve puzzle l@((i, j):cs) = do
  v <- readSudoku puzzle i j
  if v == 0
    then available puzzle i j >>= solve' puzzle l
    else solve puzzle cs
  where
    solve' _ _ [] = return Nothing
    solve' puzzle l@((i, j):cs) (v:vs) = do
      writeSudoku puzzle i j v
      solve puzzle cs >>=
        maybe
          (writeSudoku puzzle i j 0 >>= const (solve' puzzle l vs))
          (return . Just)
