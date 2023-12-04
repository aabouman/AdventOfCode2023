module Main where

import Data.Array
import Data.Char (isDigit, isSymbol, isAlphaNum)
import Data.Maybe

type Matrix a = Array Int (Array Int a)

getDimensions :: Matrix a -> (Int, Int)
getDimensions matrix = (numRows, numCols)
  where
    (_, numRows) = bounds matrix
    numCols = if numRows == 0 then 0 else let (_, c) = bounds (matrix ! 1) in c

getElement :: (Int, Int) -> Matrix a -> a
getElement (row, col) matrix = (matrix ! row) ! col

createMatrixFromList :: [[a]] -> Matrix a
createMatrixFromList ls = listArray (1, numRows) (map (listArray (1, numCols)) ls)
  where
    numRows = length ls
    numCols = if numRows == 0 then 0 else length (head ls)

isSymbolAdjacent :: (Int, (Int, Int)) -> Matrix Char -> Bool
isSymbolAdjacent (row, (colStart, colEnd)) matrix
    | colStart > colEnd || nRows == 0 || nCols == 0 = False
    | otherwise = any (\c -> any (\r -> isValidSymbol (r, c)) searchRows) searchCols
    where
        isValidSymbol (r, c) = not (isAlphaNum (getElement (r, c) matrix)) && (getElement (r, c) matrix /= '.')
        (nRows, nCols) = getDimensions matrix
        searchRows = [max (row-1) 1 .. min (row+1) nRows]
        searchCols = [max (colStart-1) 1 .. min (colEnd+1) nCols]

parseLeadingInt :: (Int, Int) -> Matrix Char -> Maybe (Int, Int)
parseLeadingInt (row, col) matrix
    | null digits = Nothing
    | otherwise = Just (read digits, length digits)
  where
    (nRows, nCols) = getDimensions matrix
    digits = takeWhile isDigit $ map (\c -> getElement (row, c) matrix) [col..nCols]

parsePartNumbers :: Matrix Char -> [[Int]]
parsePartNumbers matrix = map (\r -> partNumbers (r, 1)) [1..nRows]
  where
    partNumbers (row, col)
        | col <= 0 || col > nCols || row <= 0 || row > nRows = []
        | otherwise = case parseLeadingInt (row, col) matrix of
            Nothing        -> partNumbers (row, col+1)
            Just (a, lenA) -> if isSymbolAdjacent (row, (col,col+(lenA-1))) matrix
                              then a:partNumbers (row, col+lenA)
                              else partNumbers (row, col+lenA)
    (nRows, nCols) = getDimensions matrix

solve1 :: Matrix Char -> Int
solve1 matrix = sum (map sum (parsePartNumbers matrix))

parseLeadingIntList :: [Char] -> Maybe (Int, [Char])
parseLeadingIntList str = case span isDigit str of
    ("", _)      -> Nothing
    (digits, as) -> Just (read digits ::Int, as)

parseRowGearRatio :: [Char] -> [Int]
parseRowGearRatio [] = []
parseRowGearRatio (a:as) = case parseLeadingIntList (a:as) of
    Nothing -> parseRowGearRatio as
    Just (b, '*':bs) -> case parseLeadingIntList bs of
        Nothing -> parseRowGearRatio bs
        Just (c, cs) -> b*c:parseRowGearRatio as
    Just (b, bs) -> parseRowGearRatio bs

main :: IO ()
main = do
    input <- readFile "data/input1.txt"
    let linesOfFiles = lines input
    let mat = createMatrixFromList linesOfFiles

    print (solve1 mat)

    print (parseRowGearRatio (head linesOfFiles))

    -- print mat
