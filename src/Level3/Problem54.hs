{-# LANGUAGE LambdaCase #-}

module Level3.Problem54
  ( problem
  ) where

import Data.Char
import Data.List
import Data.Maybe

import Level3.Problem54Games
import Problem

problem :: Problem Integer
problem =
  Problem
    54
    "Poker hands"
    (toInteger $ length $ filter (== "P1") $ map checkWinner games)

-- Perhaps not an ideal solution but I opted to use this problem as an
-- exercises in haskell data types
data Hand
  = RoyalFlush
  | StraightFlush Int
  | FourOfAKind Int
                Int
  | FullHouse Int
              Int
  | Flush Int
          Int
          Int
          Int
          Int
  | Straight Int
  | ThreeOfAKind Int
                 Int
                 Int
  | TwoPairs Int
             Int
             Int
  | Pair Int
         Int
         Int
         Int
  | HighCard Int
             Int
             Int
             Int
             Int
  | Illegal
  deriving (Show, Eq)

instance Ord Hand where
  compare RoyalFlush RoyalFlush = EQ
  compare _ RoyalFlush = LT
  compare RoyalFlush _ = GT
  compare (StraightFlush c) (StraightFlush c') = compare c c'
  compare _ (StraightFlush _) = LT
  compare (StraightFlush _) _ = GT
  compare (FourOfAKind c1 c2) (FourOfAKind c1' c2') =
    successiveCompare [c1, c2] [c1', c2']
  compare _ (FourOfAKind _ _) = LT
  compare (FourOfAKind _ _) _ = GT
  compare (FullHouse t p) (FullHouse t' p') = successiveCompare [t, p] [t', p']
  compare _ (FullHouse _ _) = LT
  compare (FullHouse _ _) _ = GT
  compare (Flush c1 c2 c3 c4 c5) (Flush c1' c2' c3' c4' c5') =
    successiveCompare [c1, c2, c3, c4, c5] [c1', c2', c3', c4', c5']
  compare _ Flush {} = LT
  compare Flush {} _ = GT
  compare (Straight c) (Straight c') = compare c c'
  compare _ (Straight _) = LT
  compare (Straight _) _ = GT
  compare (ThreeOfAKind c1 c2 c3) (ThreeOfAKind c1' c2' c3') =
    successiveCompare [c1, c2, c3] [c1', c2', c3']
  compare _ ThreeOfAKind {} = LT
  compare ThreeOfAKind {} _ = GT
  compare (TwoPairs c1 c2 c3) (TwoPairs c1' c2' c3') =
    successiveCompare [c1, c2, c3] [c1', c2', c3']
  compare _ TwoPairs {} = LT
  compare TwoPairs {} _ = GT
  compare (Pair c1 c2 c3 c4) (Pair c1' c2' c3' c4') =
    successiveCompare [c1, c2, c3, c4] [c1', c2', c3', c4']
  compare _ Pair {} = LT
  compare Pair {} _ = GT
  compare (HighCard c1 c2 c3 c4 c5) (HighCard c1' c2' c3' c4' c5') =
    successiveCompare [c1, c2, c3, c4, c5] [c1', c2', c3', c4', c5']
  compare Illegal Illegal = EQ
  compare Illegal _ = LT
  compare _ Illegal = LT

data Suit
  = Heart
  | Diamond
  | Club
  | Spade
  deriving (Show, Eq, Ord)

data Card = Card
  { face :: Int
  , suit :: Suit
  } deriving (Show, Eq)

instance Ord Card where
  compare Card {face = x, suit = s} Card {face = x', suit = s'}
    | x' == x = compare s s'
    | otherwise = compare x x'

successiveCompare :: Ord a => [a] -> [a] -> Ordering
successiveCompare [] [] = EQ
successiveCompare (x:xs) (x':xs') =
  if x == x'
    then successiveCompare xs xs'
    else compare x x'

flatten5 :: (t1 -> t1 -> t1 -> t1 -> t1 -> t) -> [t1] -> t
flatten5 f l = f (head l) (l !! 1) (l !! 2) (l !! 3) (l !! 4)

apply :: t1 -> [t -> Maybe t1] -> t -> t1
apply def [] _ = def
apply def (f:fs) v = fromMaybe (apply def fs v) $ f v

evaluateHand :: [Card] -> Hand
evaluateHand cards
  | length cards /= 5 = Illegal
  | otherwise =
    apply
      (flatten5 HighCard $ sortBy (flip compare) $ faceValues cards)
      [ royalFlush
      , straightFlush
      , fourOfAKind
      , fullHouse
      , flush
      , straight
      , threeOfAKind
      , twoPairs
      , pair
      ]
      cards

royalFlush :: [Card] -> Maybe Hand
royalFlush cards =
  straightFlush cards >>= \case
    StraightFlush 14 -> Just RoyalFlush
    _ -> Nothing

straightFlush :: [Card] -> Maybe Hand
straightFlush cards =
  flush cards >>= \_ ->
    straight cards >>= \case
      Straight x -> Just $ StraightFlush x
      _ -> Nothing

fourOfAKind :: [Card] -> Maybe Hand
fourOfAKind cards =
  quadrupleVal >>= \x -> Just $ FourOfAKind x (head $ filter (/= x) sorted)
  where
    sorted = sortBy (flip compare) $ faceValues cards
    quadruple = filter ((== 4) . length) $ group sorted
    quadrupleVal =
      if not (null quadruple)
        then Just $ head $ head quadruple
        else Nothing

fullHouse :: [Card] -> Maybe Hand
fullHouse cards =
  threeOfAKind cards >>= \case
    ThreeOfAKind t c1 c2 ->
      if c1 == c2
        then Just $ FullHouse t c1
        else Nothing
    _ -> Nothing

flush :: [Card] -> Maybe Hand
flush cards =
  if all ((== s) . suit) cards
    then Just $ flatten5 Flush sorted
    else Nothing
  where
    s = suit $ head cards
    sorted = sortBy (flip compare) $ faceValues cards

straight :: [Card] -> Maybe Hand
straight cards =
  if dif1 sorted
    then Just $ Straight $ head sorted
    else Nothing
  where
    sorted = sortBy (flip compare) $ faceValues cards
    dif1 [_] = True
    dif1 (x1:x2:xs) = (x1 - x2 == 1) && dif1 (x2 : xs)

threeOfAKind :: [Card] -> Maybe Hand
threeOfAKind cards =
  tripleVal >>= \t -> Just $ ThreeOfAKind t (head $ filt t) (filt t !! 1)
  where
    filt t = filter (/= t) sorted
    sorted = sortBy (flip compare) $ faceValues cards
    triple = filter ((== 3) . length) $ group sorted
    tripleVal =
      if not (null triple)
        then Just $ head $ head triple
        else Nothing

twoPairs :: [Card] -> Maybe Hand
twoPairs cards =
  doubleVals >>= \(p1, p2) ->
    Just $ TwoPairs p1 p2 (head $ filter (/= p1) $ filter (/= p2) sorted)
  where
    sorted = sortBy (flip compare) $ faceValues cards
    doubles = map head $ filter ((== 2) . length) $ group sorted
    doubleVals =
      if length doubles > 1
        then Just (head doubles, doubles !! 1)
        else Nothing

pair :: [Card] -> Maybe Hand
pair cards =
  doubleVal >>= \p -> Just $ Pair p (head $ filt p) (filt p !! 1) (filt p !! 2)
  where
    filt t = filter (/= t) sorted
    sorted = sortBy (flip compare) $ faceValues cards
    doubles = map head $ filter ((== 2) . length) $ group sorted
    doubleVal =
      if not (null doubles)
        then Just $ head doubles
        else Nothing

faceValues :: [Card] -> [Int]
faceValues = map face

parseFace :: Char -> Maybe Int
parseFace 'A' = Just 14
parseFace 'K' = Just 13
parseFace 'Q' = Just 12
parseFace 'J' = Just 11
parseFace 'T' = Just 10
parseFace f =
  if isDigit f
    then Just $ digitToInt f
    else Nothing

parseSuit :: Char -> Maybe Suit
parseSuit 'C' = Just Club
parseSuit 'D' = Just Diamond
parseSuit 'S' = Just Spade
parseSuit 'H' = Just Heart
parseSuit _ = Nothing

parseCard :: String -> Card
parseCard c@[c1, c2] =
  fromMaybe (error ("Could not parse " ++ c)) $
  parseFace c1 >>= \f -> parseSuit c2 >>= Just . Card f
parseCard c = error ("Could not parse " ++ c)

parseHand :: String -> [Card]
parseHand = map parseCard . words

checkWinner :: String -> String
checkWinner s =
  case compare (evS p1) (evS p2) of
    GT -> "P1"
    LT -> "P2"
    EQ -> "DR"
  where
    evS = evaluateHand . parseHand
    (p1, p2) = splitAt 14 s
