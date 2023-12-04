module Main where

import Data.Array
import Data.Char (isDigit, isAlphaNum)
import Data.Maybe

-- Lower/Upper Bounds
data Bound = Bound { lower :: Int, upper :: Int}
  deriving Show
-- A entry on the character grid. A Part has a serial number and Symbol a defining symbol.
-- Both types specify a row and column span.
data Entry = Part {num :: Int, rows :: Bound, cols :: Bound}
           | Symbol {char :: Char, rows :: Bound, cols :: Bound}
  deriving Show
isSymbolEntry :: Entry -> Bool
isSymbolEntry entry = case entry of Symbol {} -> True; _ -> False

-- Define a L-inf norm for distances on the character grid.
class Distance a where
  dist :: a -> a -> Int
instance Distance Int where
  dist a b = abs (a - b)
instance Distance Bound where
  dist (Bound lb1 up1) (Bound lb2 up2) = min (dist lb1 up2) (dist lb2 up1)
instance Distance Entry where
  dist e1 e2 = max (dist (rows e1) (rows e2)) (dist (cols e1) (cols e2))

isValidSymbol :: Char -> Bool
isValidSymbol c = not (isAlphaNum c) && c /= '.'

-- Parses a integer which begins a string, "123Hello" -> (123, "Hello")
parseLeadingInt :: [Char] -> Maybe (Int, [Char])
parseLeadingInt str = case span isDigit str of
  ("", _)      -> Nothing
  (digits, as) -> Just (read digits ::Int, as)

-- Parses all Symbols in a row.
parseRowSymbols :: Int -> [Char] -> [Entry]
parseRowSymbols rowInd = parseRowSymbols' rowInd 1
  where parseRowSymbols' _ _ [] = []
        parseRowSymbols' r c (a:as) = if isValidSymbol a then
                                        Symbol a (Bound r r) (Bound c c) : parseRowSymbols' r (c+1) as
                                      else
                                        parseRowSymbols' r (c+1) as
-- Parses all Parts in a row.
parseRowParts :: Int -> [Char] -> [Entry]
parseRowParts rowInd = parseRowParts' rowInd 1
  where parseRowParts' _ _ [] = []
        parseRowParts' r c (a:as) = case parseLeadingInt (a:as) of
                                      Nothing -> parseRowParts' r (c+1) as
                                      Just (num, bs) ->
                                        let numLen = length (a:as) - length bs
                                        in Part num (Bound r r) (Bound c (c+numLen-1)) : parseRowParts' r (c+numLen) bs

parse :: [[Char]] -> [Entry]
parse lines = concat (zipWith parseEntries [0..] lines)
  where parseEntries r line = parseRowSymbols r line ++ parseRowParts r line

-- Sum up all valid serial numbers.
solve1 :: [Entry] -> Int
solve1 entries = foldr (\(Part num _ _) b -> num+b) 0 validParts
  where
    symbols = filter isSymbolEntry entries             -- Filter out only symbols.
    parts = filter (not . isSymbolEntry) entries       -- Filter out only parts.
    validParts = filter isValidPart parts              -- Get valid parts.
    -- Valid part is one which has a distance of 1 to a symbol.
    isValidPart part = any (\sym -> dist part sym == 1) symbols

-- Sum up all gear ratios.
solve2 :: [Entry] -> Int
solve2 entries = sum (map (foldr (\(Part num _ _) b -> num*b) 1 . partNeighbors) gears)
  where
    parts = filter (not . isSymbolEntry) entries      -- Separate all parts and all
    symbols = filter isSymbolEntry entries
    gears = filter (\sym -> isStarSymbol sym && length (partNeighbors sym) >= 2) symbols
    isStarSymbol entry = case entry of Symbol '*' _ _ -> True; _ -> False
    -- For each symbol check if it has neighbors
    partNeighbors symbol = filter (\part -> dist symbol part == 1) parts

main :: IO ()
main = do
    input <- readFile "data/input1.txt"
    let linesOfFiles = lines input

    print (solve1 (parse linesOfFiles))
    print (solve2 (parse linesOfFiles))