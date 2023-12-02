module Main where

import Data.List ()
import Data.Char (isDigit, digitToInt)

calibrationInt1 :: [Char] -> Int
calibrationInt1 as = read ((\x -> [head x, last x]) (filter isDigit as)) ::Int

calibrationInt2 :: [Char] -> Int
calibrationInt2 as = (\x -> 10 * head x + last x) (calibrationIntHelper as)
    where
        calibrationIntHelper ('z':'e':'r':'o':as)     = 0:calibrationIntHelper ('e':'r':'o':as)
        calibrationIntHelper ('o':'n':'e':as)         = 1:calibrationIntHelper ('n':'e':as)
        calibrationIntHelper ('t':'w':'o':as)         = 2:calibrationIntHelper ('w':'o':as)
        calibrationIntHelper ('t':'h':'r':'e':'e':as) = 3:calibrationIntHelper ('h':'r':'e':'e':as)
        calibrationIntHelper ('f':'o':'u':'r':as)     = 4:calibrationIntHelper ('o':'u':'r':as)
        calibrationIntHelper ('f':'i':'v':'e':as)     = 5:calibrationIntHelper ('i':'v':'e':as)
        calibrationIntHelper ('s':'i':'x':as)         = 6:calibrationIntHelper ('i':'x':as)
        calibrationIntHelper ('s':'e':'v':'e':'n':as) = 7:calibrationIntHelper ('e':'v':'e':'n':as)
        calibrationIntHelper ('e':'i':'g':'h':'t':as) = 8:calibrationIntHelper ('i':'g':'h':'t':as)
        calibrationIntHelper ('n':'i':'n':'e':as)     = 9:calibrationIntHelper ('i':'n':'e':as)
        calibrationIntHelper (a:as) = if isDigit a then digitToInt a:calibrationIntHelper as else calibrationIntHelper as
        calibrationIntHelper [] = []

main :: IO ()
main = do
    input <- readFile "data/input1.txt"
    let linesOfFiles = lines input
    let result1 = sum (map calibrationInt1 linesOfFiles)
    print result1

    input <- readFile "data/input1.txt"
    let linesOfFiles = lines input
    let result2 = sum (map calibrationInt2 linesOfFiles)
    print result2