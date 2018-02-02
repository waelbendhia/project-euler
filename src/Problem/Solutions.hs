module Problem.Solutions
  ( getProblem
  ) where

import qualified Problem.Level1.Problem1 as P1
import qualified Problem.Level1.Problem10 as P10
import qualified Problem.Level1.Problem11 as P11
import qualified Problem.Level1.Problem12 as P12
import qualified Problem.Level1.Problem13 as P13
import qualified Problem.Level1.Problem14 as P14
import qualified Problem.Level1.Problem15 as P15
import qualified Problem.Level1.Problem16 as P16
import qualified Problem.Level1.Problem17 as P17
import qualified Problem.Level1.Problem18 as P18
import qualified Problem.Level1.Problem19 as P19
import qualified Problem.Level1.Problem2 as P2
import qualified Problem.Level1.Problem20 as P20
import qualified Problem.Level1.Problem21 as P21
import qualified Problem.Level1.Problem22 as P22
import qualified Problem.Level1.Problem23 as P23
import qualified Problem.Level1.Problem24 as P24
import qualified Problem.Level1.Problem25 as P25
import qualified Problem.Level1.Problem3 as P3
import qualified Problem.Level1.Problem4 as P4
import qualified Problem.Level1.Problem5 as P5
import qualified Problem.Level1.Problem6 as P6
import qualified Problem.Level1.Problem7 as P7
import qualified Problem.Level1.Problem8 as P8
import qualified Problem.Level1.Problem9 as P9
import qualified Problem.Level2.Problem26 as P26
import qualified Problem.Level2.Problem27 as P27
import qualified Problem.Level2.Problem28 as P28
import qualified Problem.Level2.Problem29 as P29
import Problem.Problem

level1Problems :: [Problem Integer]
level1Problems =
  [ P1.problem
  , P2.problem
  , P3.problem
  , P4.problem
  , P5.problem
  , P6.problem
  , P7.problem
  , P8.problem
  , P9.problem
  , P10.problem
  , P11.problem
  , P12.problem
  , P13.problem
  , P14.problem
  , P15.problem
  , P16.problem
  , P17.problem
  , P18.problem
  , P19.problem
  , P20.problem
  , P21.problem
  , P22.problem
  , P23.problem
  , P24.problem
  , P25.problem
  , P26.problem
  , P27.problem
  , P28.problem
  , P29.problem
  ]

getProblem :: Int -> Problem Integer
getProblem i =
  if length matching == 0
    then Problem i "Not solved yet" 0
    else head $ matching
  where
    matching = filter ((== i) . ind) $ concat [level1Problems]
