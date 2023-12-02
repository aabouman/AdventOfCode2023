module Main where

import Data.Char (isDigit, digitToInt)
import Data.Maybe

data GameSet = GameSet { red :: Int, green :: Int, blue :: Int }
    deriving (Show, Eq)
data Game = Game {index :: Int, gameSets :: [GameSet]}
    deriving Show

instance Num GameSet where
    (+) (GameSet r1 g1 b1) (GameSet r2 g2 b2) = GameSet (r1 + r2) (g1 + g2) (b1 + b2)
    (-) = undefined
    (*) = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined

class Validity a where
    isValid :: GameSet -> a -> Bool
instance Validity GameSet where
    isValid (GameSet r1 g1 b1) (GameSet r2 g2 b2) = not (r2 > r1 || g2 > g1 || b2 > b1)
instance Validity Game where
    isValid limits (Game _ gameSets) = all (isValid limits) gameSets

-- Parses a integer which begins a string, "123Hello" -> (123, "Hello")
parseLeadingInt :: [Char] -> Maybe (Int, [Char])
parseLeadingInt str = case span isDigit str of
    ("", _)      -> Nothing
    (digits, as) -> Just (read digits ::Int, as)

-- Parses a single "drawing" into a GameSet, "123 red, 13 green" -> (GameSet 123 0 0, ", 13 green")
drawingParser :: [Char] -> Maybe (GameSet, [Char])
drawingParser str = case parseLeadingInt str of
    Nothing -> Nothing
    Just (a, as) -> drawingParser' as
        where drawingParser' (' ':'r':'e':'d':bs)         = Just (GameSet a 0 0, bs)
              drawingParser' (' ':'g':'r':'e':'e':'n':bs) = Just (GameSet 0 a 0, bs)
              drawingParser' (' ':'b':'l':'u':'e':bs)     = Just (GameSet 0 0 a, bs)
              drawingParser' _                            = Nothing

-- Parses a complete game set closed by a ';',
-- "123 red, 3 blue, 13 green, 3 blue; Hello" -> (GameSet 123 12 6, " Hello")
gameSetParser :: [Char] -> (GameSet, [Char])
gameSetParser [] = (GameSet 0 0 0, [])
gameSetParser (';':as) = (GameSet 0 0 0, as)
gameSetParser (a:as) = case drawingParser (a:as) of
    Nothing -> gameSetParser as
    Just (gameSet, bs) -> let tmp = gameSetParser bs
                          in (gameSet + fst tmp, snd tmp)

-- Parses a complete game closed by a ';',
-- "Game 34: 123 red, 3 blue, 13 green, 3 blue;" -> Game 34 (GameSet 123 12 6)
gameParser :: [Char] -> Maybe Game
gameParser ('G':'a':'m':'e':' ':as) = case parseLeadingInt as of
    Nothing -> Nothing
    Just (b, bs) -> Just (Game b (gameParserHelper bs))
    where gameParserHelper as = case gameSetParser as of
              (gameSet, []) -> [gameSet]
              (gameSet, bs) -> gameSet:gameParserHelper bs
gameParser _ = Nothing

-- Solves problem 1
solve1 :: [Maybe Game] -> Int
solve1 games = sumFailureIds' games
    where limits = GameSet 12 13 14
          sumFailureIds' [] = 0
          sumFailureIds' (Nothing:games) = sumFailureIds' games
          sumFailureIds' (Just game:games)
            | isValid limits game = index game + sumFailureIds' games
            | otherwise = sumFailureIds' games

supremum :: GameSet -> GameSet -> GameSet
supremum (GameSet r1 g1 b1) (GameSet r2 g2 b2) = GameSet (max r1 r2) (max g1 g2) (max b1 b2)

leastUpperBound :: Game -> GameSet
leastUpperBound game = foldr supremum (GameSet 0 0 0) (gameSets game)

solve2 :: [Maybe Game] -> Int
solve2 = foldr (\a b -> unwrap a + b) 0
    where unwrap = maybe 0 (pow . leastUpperBound)
          pow gameSet = red gameSet * green gameSet * blue gameSet

main :: IO ()
main = do
    input <- readFile "data/input1.txt"
    let linesOfFiles = lines input
    let result = gameParser (head linesOfFiles)
    let limits = GameSet 12 13 14
    let games = map gameParser linesOfFiles

    print (solve1 games)
    print (solve2 games)
