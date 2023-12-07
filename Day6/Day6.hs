module Main where

import Data.Fixed

data Race = Race {time :: Int, distance ::Int}
  deriving Show
data RaceStrategy = RaceStrategy {race :: Race, holdDuration :: Int}
  deriving Show

-- IEEE floating point precision.
epsilon :: Double
epsilon = 2**(-52)

parseTimes :: [Char] -> [Int]
parseTimes('T':'i':'m':'e':':':as) = Prelude.map read $ words as :: [Int]
parseDistances :: [Char] -> [Int]
parseDistances('D':'i':'s':'t':'a':'n':'c':'e':':':as) = Prelude.map read $ words as :: [Int]
parse :: [Char] -> [Race]
parse str = (\[a,b]-> zipWith Race (parseTimes a) (parseDistances b)) ls
  where ls = take 2 (lines str)

countWinningStrategies :: Race -> Int
countWinningStrategies (Race t d) = countIntegersInInterval lb ub
  where ub = (fromIntegral t + sqrt (fromIntegral (-4 * d + t^2))) / 2
        lb = (fromIntegral t - sqrt (fromIntegral (-4 * d + t^2))) / 2
        -- Count number of integers in interval non-inclusive.
        -- Uses floating point precision to avoid including end points when Int.
        countIntegersInInterval a b = max 0 (floor (b - 10 * epsilon) - ceiling (a + 10 * epsilon) + 1)

solve1 :: [Race] -> Int
solve1 races = product (map countWinningStrategies races)

parseTime2 :: [Char] -> Int
parseTime2('T':'i':'m':'e':':':as) = read $ as :: Int
parseDistance2 :: [Char] -> Int
parseDistance2('D':'i':'s':'t':'a':'n':'c':'e':':':as) = read $ as :: Int
parse2 :: [Char] -> Race
parse2 str = (\[a, b]-> Race (parseTime2 (filter (/= ' ') a)) (parseDistance2 (filter (/= ' ') b))) ls
  where ls = take 2 (lines str)

main :: IO ()
main = do
  input <- readFile "data/input1.txt"
  let races = parse input

  print races
  print (map countWinningStrategies races)
  print (solve1 races)

  let race = parse2 input
  print race
  print (countWinningStrategies race)
