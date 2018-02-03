module Main where

import Problem
import Solutions
import System.TimeIt
import Text.Read

main :: IO ()
main = do
  num <- promptInt "Select a problem"
  timeIt $ putStrLn $ prettyPrint $ getProblem num
  main

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
