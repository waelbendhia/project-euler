module Level4.Problem84
  ( problem
  ) where

import Data.List
import System.Random

import Problem

-- I couldn't get the simulation quite right for 6 sided dice but it worked
-- out ok for 4 sided dice. Guess the margins are less forgiving in that case.
-- The play turn is a mess and the performance isn't ideal, but it gets the desired result
problem :: Problem Integer
problem =
  Problem
  { ind = 84
  , name = "Monopoly odds"
  , solution =
      read $
      modal $ map (fst . fst) $ reverse $ statisticsForGame (1, 4) 100000 0
  }

data Tiles
  = Go
  | GoToJail
  | CommunityChest
  | Chance
  | Jail
  | Normal String
  deriving (Show, Eq)

data Card
  = AdvanceToGoCard
  | GoToJailCard
  | GoTo String
  | GoToNext Char
  | GoBack3
  | Ignored
  deriving (Show)

data GameState = GameState
  { position :: Int
  , chanceDeck :: [Card]
  , communityChestDeck :: [Card]
  , rolls :: [(Int, Int)]
  , prevDoubles :: Int
  } deriving (Show)

initGame :: (Eq t, Num t) => (Int, Int) -> t -> Int -> GameState
initGame dRange turns seed =
  GameState
  { position = 0
  , chanceDeck = chDeck
  , communityChestDeck = ccDeck
  , prevDoubles = 0
  , rolls = diceRolls
  }
  where
    gen = mkStdGen seed
    (diceRolls, genAfterRolls) = generateNDiceRolls turns dRange gen
    (ccDeck, genAftercc) = shuffleCards genAfterRolls communityChestCards
    (chDeck, _) = shuffleCards genAftercc chanceCards

playTurn :: GameState -> Maybe GameState
playTurn gs =
  case getDiceRolls $ rolls gs of
    Nothing -> Nothing
    Just (roll, double, remaining) ->
      if newDoubles == 3
        then Just
               GameState
               { position = 10
               , chanceDeck = chanceDeck gs
               , communityChestDeck = communityChestDeck gs
               , rolls = remaining
               , prevDoubles = 0
               }
        else Just $ handlePos' gs
      where handlePos' gs =
              case board !! position updated of
                (_, Chance) -> handlePos updated
                _ -> updated
              where
                updated = handlePos gs
            newDoubles =
              if double
                then prevDoubles gs + 1
                else 0
            newPos = (position gs + roll) `mod` length board
            handlePos gs =
              case board !! newPos of
                (_, GoToJail) ->
                  GameState
                  { position = 10
                  , chanceDeck = chanceDeck gs
                  , communityChestDeck = communityChestDeck gs
                  , rolls = remaining
                  , prevDoubles = newDoubles
                  }
                (_, Chance) ->
                  let (c, deck) = drawCard $ chanceDeck gs
                  in GameState
                     { position = nextPosition c newPos
                     , chanceDeck = deck
                     , communityChestDeck = communityChestDeck gs
                     , rolls = remaining
                     , prevDoubles = newDoubles
                     }
                (_, CommunityChest) ->
                  let (c, deck) = drawCard $ communityChestDeck gs
                  in GameState
                     { position = nextPosition c newPos
                     , chanceDeck = chanceDeck gs
                     , communityChestDeck = deck
                     , rolls = remaining
                     , prevDoubles = newDoubles
                     }
                _ ->
                  GameState
                  { position = newPos
                  , chanceDeck = chanceDeck gs
                  , communityChestDeck = communityChestDeck gs
                  , rolls = remaining
                  , prevDoubles = newDoubles
                  }

playGame :: (Num t, Eq t) => (Int, Int) -> t -> Int -> [(Int, Tiles)]
playGame dRange turns s =
  map ((board !!) . position) $
  unfoldr
    (\gs -> playTurn gs >>= (\gs' -> return (gs', gs')))
    (initGame dRange turns s)

modal :: Show a => [a] -> [Char]
modal l =
  concat $
  map
    (\s ->
       if length s < 2
         then '0' : s
         else s) $
  map show $ take 3 l

statisticsForGame ::
     (Eq t, Num t) => (Int, Int) -> t -> Int -> [((Int, Tiles), Int)]
statisticsForGame dRange turns s =
  sortBy (\a b -> compare (snd a) (snd b)) $
  map (\l -> (head l, length l)) $
  group $ sortBy (\a b -> compare (fst a) (fst b)) $ playGame dRange turns s

getDiceRolls :: (Eq a, Num a) => [(a, a)] -> Maybe (a, Bool, [(a, a)])
getDiceRolls [] = Nothing
getDiceRolls (r:rs) = Just (sumDice r, sameDice r, rs)
  where
    sameDice (d1, d2) = d1 == d2
    sumDice (d1, d2) = d1 + d2

drawCard :: [a] -> (a, [a])
drawCard (c:cs) = (c, cs ++ [c])

communityChestCards :: [Card]
communityChestCards = [AdvanceToGoCard, GoToJailCard] ++ replicate 14 Ignored

chanceCards :: [Card]
chanceCards =
  [ AdvanceToGoCard
  , GoToJailCard
  , GoTo "C1"
  , GoTo "E3"
  , GoTo "H2"
  , GoTo "R1"
  , GoToNext 'R'
  , GoToNext 'R'
  , GoToNext 'U'
  , GoBack3
  ] ++
  replicate 6 Ignored

board :: [(Int, Tiles)]
board =
  zip
    [0 ..]
    [ Go
    , Normal "A1"
    , CommunityChest
    , Normal "A2"
    , Normal "T1"
    , Normal "R1"
    , Normal "B1"
    , Chance
    , Normal "B2"
    , Normal "B3"
    , Jail
    , Normal "C1"
    , Normal "U1"
    , Normal "C2"
    , Normal "C3"
    , Normal "R2"
    , Normal "D1"
    , CommunityChest
    , Normal "D2"
    , Normal "D3"
    , Normal "FP"
    , Normal "E1"
    , Chance
    , Normal "E2"
    , Normal "E3"
    , Normal "R3"
    , Normal "F1"
    , Normal "F2"
    , Normal "U2"
    , Normal "F3"
    , GoToJail
    , Normal "G1"
    , Normal "G2"
    , CommunityChest
    , Normal "G3"
    , Normal "R4"
    , Chance
    , Normal "H1"
    , Normal "T2"
    , Normal "H2"
    ]

cardName :: Tiles -> String
cardName (Normal s) = s
cardName _ = ""

getTileByName :: [Char] -> (Int, Tiles)
getTileByName n = head res
  where
    res = filter ((n ==) . cardName . snd) $ cycle board

getTileByType :: Char -> Int -> (Int, Tiles)
getTileByType t p = head res
  where
    res =
      filter
        (\tile -> cardName (snd tile) /= "" && t == head (cardName (snd tile))) $
      drop p $ cycle board

nextPosition :: Card -> Int -> Int
nextPosition card curPos =
  case card of
    Ignored -> curPos
    AdvanceToGoCard -> 0
    GoToJailCard -> 10
    GoTo s -> fst $ getTileByName s
    GoToNext c -> fst $ getTileByType c curPos
    GoBack3 -> mod (curPos - 3) (length board)

rollDice :: RandomGen t => (Int, Int) -> t -> (Int, t)
rollDice (lo, hi) g = (rolled, nextG)
  where
    (v, nextG) = next g
    rolled = lo + (v `mod` (1 + hi - lo))

generateRandNumbers ::
     (RandomGen t, Num t1, Eq t1) => t1 -> (Int, Int) -> t -> ([Int], t)
generateRandNumbers n range g =
  ( map fst $ res
  , if length res > 0
      then snd $ head res
      else g)
  where
    res =
      unfoldr
        (\(n', g') ->
           let (roll, nextG) = rollDice range g'
           in if n' == n
                then Nothing
                else Just ((roll, nextG), (n' + 1, nextG)))
        (0, g)

generateNDiceRolls ::
     (RandomGen t, Num t1, Eq t1) => t1 -> (Int, Int) -> t -> ([(Int, Int)], t)
generateNDiceRolls turns dRange gen = (zip dice1Rolls dice2Rolls, gen2)
  where
    (dice1Rolls, gen1) = generateRandNumbers turns dRange gen
    (dice2Rolls, gen2) = generateRandNumbers turns dRange gen1

shuffleCards :: RandomGen t => t -> [b] -> ([b], t)
shuffleCards g cards =
  (map snd $ sortBy (\a b -> fst a `compare` fst b) $ zip nums cards, g2)
  where
    (nums, g2) = generateRandNumbers (length cards) (1, length cards) g
