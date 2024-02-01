module Main where

import Debug.Trace (trace)

import Data.List (group, sort, sortBy)
import Data.Ord (comparing)
import Data.Map.Internal.Debug (ordered)

-- data Suit = Hearts | Diamonds | Clubs | Spades
--     deriving (Show, Enum, Eq)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Show, Enum, Eq, Ord)
newtype Card = Card { {-suit :: Suit,-} rank :: Rank }
    deriving (Show, Ord, Eq)
newtype Hand = Hand [Card]
    deriving (Show, Eq)
instance Ord Hand where
  compare hand1 hand2 = trace ("myPureFunction called with:\n\t" ++ show hand1 ++ "\n\t" ++ show hand2) compare (score hand1) (score hand2)

data Score = FiveOfKind Card | FourOfKind {one ::Card, four :: Card}
           | FullHouse {two ::Card, three :: Card}
           | ThreeOfKind {one1 ::Card, one2 ::Card, three :: Card}
           | TwoPair {one :: Card, two1 :: Card, two2 :: Card}
           | OnePair {one1 :: Card, one2 :: Card, one3 :: Card, two :: Card}
           | High {one1 :: Card, one2 :: Card, one3 :: Card, one4 :: Card, one5 :: Card}
    deriving (Show, Eq)

score :: Hand -> Score
score (Hand cards) = case uniqueCardCounts of
  [(card, 5)] -> FiveOfKind card
  [(card1, 1), (card4, 4)] -> FourOfKind card1 card4
  [(card2, 2), (card3, 3)] -> FullHouse card2 card3
  [(card11, 1), (card12, 1), (card3, 3)] -> ThreeOfKind card11 card12 card3
  [(card1, 1), (card21, 2), (card22, 2)] -> TwoPair card1 card21 card22
  [(card11, 1), (card12, 1), (card13, 1), (card2, 2)] -> OnePair card11 card12 card13 card2
  [(card11, 1), (card12, 1), (card13, 1), (card14, 1), (card15, 1)] -> High card11 card12 card13 card14 card15
  where uniqueCardCounts = sortBy (comparing snd) $ map (\g -> (head g, length g)) $ group $ sort cards

instance Ord Score where
  compare (FiveOfKind a1) (FiveOfKind b1) = compare a1 b1
  compare (FourOfKind a1 b1) (FourOfKind a2 b2) =
    case compare b1 b2 of EQ -> compare (FiveOfKind a1) (FiveOfKind b1); val -> val
  compare (FullHouse a1 b1) (FullHouse a2 b2) = compare (FourOfKind a1 b1)  (FourOfKind a2 b2)
  compare (ThreeOfKind a1 b1 c1) (ThreeOfKind a2 b2 c2) =
    case compare c1 c2 of EQ -> compare (FourOfKind a1 b1)  (FourOfKind a2 b2); val -> val
  compare (TwoPair a1 b1 c1) (TwoPair a2 b2 c2) = compare (ThreeOfKind a1 b1 c1) (ThreeOfKind a2 b2 c2)
  compare (OnePair a1 b1 c1 d1) (OnePair a2 b2 c2 d2) =
    case compare d1 d2 of EQ -> compare (TwoPair a1 b1 c1) (TwoPair a2 b2 c2); val -> val
  compare (High a1 b1 c1 d1 e1) (High a2 b2 c2 d2 e2) =
    case compare e1 e2 of EQ -> compare (OnePair a1 b1 c1 d1) (OnePair a2 b2 c2 d2); val -> val
  compare _ (FiveOfKind _)   = LT
  compare (FiveOfKind _) _   = GT
  compare _ (FourOfKind _ _) = LT
  compare (FourOfKind _ _) _ = GT
  compare _ (FullHouse _ _)  = LT
  compare (FullHouse _ _) _  = GT
  compare _ (ThreeOfKind {}) = LT
  compare (ThreeOfKind {}) _ = GT
  compare _ (TwoPair {})     = LT
  compare (TwoPair {}) _     = GT
  compare _ (OnePair {})     = LT
  compare (OnePair {}) _     = GT

data Play = Play {hand :: Hand, bet :: Int}
    deriving Show

parse :: String -> Play
parse str = (\[a,b] -> Play (parseHand a) (parseBet b)) (words str)
parseBet :: String -> Int
parseBet str = read str :: Int
parseHand :: [Char] -> Hand
parseHand str = Hand (map Card (parseHand' str))
  where
    parseHand' ('2':as) = Two:parseHand' as
    parseHand' ('3':as) = Three:parseHand' as
    parseHand' ('4':as) = Four:parseHand' as
    parseHand' ('5':as) = Five:parseHand' as
    parseHand' ('6':as) = Six:parseHand' as
    parseHand' ('7':as) = Seven:parseHand' as
    parseHand' ('8':as) = Eight:parseHand' as
    parseHand' ('9':as) = Nine:parseHand' as
    parseHand' ('T':as) = Ten:parseHand' as
    parseHand' ('J':as) = Jack:parseHand' as
    parseHand' ('Q':as) = Queen:parseHand' as
    parseHand' ('K':as) = King:parseHand' as
    parseHand' ('A':as) = Ace:parseHand' as
    parseHand' _ = []

solve1 :: [Play] -> Int
solve1 plays = sum (zipWith (*) orderedBets [1..])
  where orderedPlays = sortBy (\(Play h1 _) (Play h2 _) -> compare h1 h2) plays
        orderedBets = map bet orderedPlays

main :: IO ()
main = do
  input <- readFile "data/input1.txt"
  let plays = map parse (lines input)
  let hands = map hand plays

  print (solve1 plays)

  let h1 = Hand [Card Eight, Card  King, Card  Eight, Card  Eight, Card  King]
  print h1
  let h2 = Hand [Card Queen, Card  Six, Card  Seven, Card  Ten, Card Five]
  print h2

  print (score h1)
  print (score h2)
  print (compare h1 h2)