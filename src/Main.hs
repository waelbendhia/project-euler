module Main where

import Problem
import Problem1
import Problem2
import Problem3
import Problem4
import Problem5
import Problem6
import Text.Read

main :: IO ()
main = do
  num <- promptInt "Select a problem"
  putStrLn $
    show $
    solution
      (case num of
         1 -> problem1
         2 -> problem2
         3 -> problem3
         4 -> problem4
         5 -> problem5
         6 -> problem6
         _ -> Problem 0 "Not solved yet" 0)
  retry main

always :: t1 -> t -> t1
always a _ = a

retry :: IO b -> IO b
retry a = putStrLn "Could not parse input:" >>= always a

promptLine :: String -> IO String
promptLine prompt = putStrLn prompt >>= always getLine

promptInt :: String -> IO Int
promptInt prompt = do
  ln <- promptLine prompt
  case readMaybe ln of
    Just x -> return x
    Nothing -> retry $ promptInt prompt
