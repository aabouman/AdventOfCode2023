module Main where

import Data.Char (isDigit, digitToInt)
import Data.Maybe
import Data.Semigroup
import Data.Monoid
import qualified Data.Map as Map

data Card = Card {idNum :: Int, drawings :: [Int], winningNumbers :: [Int]}
  deriving (Show, Eq)
instance Semigroup Card where
  (Card a b c) <> (Card x y z) = Card (max a x) (b ++ y) (c ++ z)
instance Monoid Card where
  mempty = Card 0 [] []
instance Ord Card where
    compare (Card a _ _) (Card b _ _) = compare a b

winCount :: Card -> Int
winCount (Card id drawings winningNumbers) = length (filter (`elem` winningNumbers) drawings)
score :: Card -> Int
score card
  | winningCount >= 1 = 2^(winningCount - 1)
  | otherwise = 0
    where winningCount = winCount card

-- Parses a integer which begins a string, "123Hello" -> (123, "Hello")
parseLeadingInt :: [Char] -> Maybe (Int, [Char])
parseLeadingInt str = case span isDigit str of
    ("", _)      -> Nothing
    (digits, as) -> Just (read digits ::Int, as)

parseIntList :: [[Char]] -> Maybe [Int]
parseIntList words = if null justInts then Nothing else Just unwrapJusts
  where justInts = takeWhile isJust (map parseLeadingInt words)
        unwrapJusts = map (\(Just (a, _)) -> a) justInts

parse :: String -> Maybe Card
parse str = parse' (words str)
  where
    parse' [] = Nothing
    parse' [_] = Nothing
    parse' (a:b:bs)
      | a == "Card" = case parseLeadingInt b of Nothing -> Nothing; Just (c, cs) -> Just (Card c [] []) <> parse' (cs:bs)
      | a == ":" = case parseIntList (b:bs) of Nothing -> Nothing; Just cs -> Just (Card 0 cs []) <> parse' (drop (length cs) (b:bs))
      | a == "|" = case parseIntList (b:bs) of Nothing -> Nothing; Just cs -> Just (Card 0 [] cs) <> parse' (drop (length cs) (b:bs))
      | otherwise = Nothing

solve1 :: [Card] -> Int
solve1 cards = sum (map score cards)

type CardCounts = Map.Map Card Int
countCards :: [Card] -> CardCounts
countCards cards = Map.fromList [(card, 1) | card <- cards]
addCards :: Int -> Card -> Map.Map Card Int -> Map.Map Card Int
addCards n = Map.adjust (+n)

countWinnings :: [Card] -> Map.Map Card Int -> Map.Map Card Int
countWinnings [] cardCounts = cardCounts
countWinnings (card:cards) cardCounts = countWinnings cards newCardCounts
  where wonCardIds = [(idNum card + 1)..(idNum card + winCount card)]
        currCardCount = fromJust (Map.lookup card cardCounts)
        newCardCounts = foldr (\idNum -> addCards currCardCount (Card idNum [] [])) cardCounts wonCardIds

solve2 :: [Card] -> Int
solve2 cards = sum (Map.elems (countWinnings cards (countCards cards)))

main :: IO ()
main = do
    input <- readFile "data/input1.txt"
    let linesOfFiles = lines input
    let cards = mapMaybe parse linesOfFiles
    let cardCounts = countCards cards

    print (solve1 cards)
    print (solve2 cards)