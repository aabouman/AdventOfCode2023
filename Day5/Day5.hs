module Main where

import Data.Maybe (fromMaybe)
import Data.Text (Text, splitOn, pack, unpack)
import Data.List (find, nub)

data Range a = Range { lb :: a, ub :: a}
  deriving Show
data AffineTransform a = AffineTransform {linear :: a, bias :: a }
  deriving Show
data BoundedAffineTransform a = BoundedAffineTransform {domain :: Range a, affineTransform :: AffineTransform a }
  deriving Show
class Transformable t a where
    applyTransform :: t -> a -> a

data SeedTransform a = SeedTransform {name :: [Char], transforms :: [BoundedAffineTransform a]}
  deriving Show

member :: Ord a => a -> Range a -> Bool
member x (Range lb ub) = lb <= x && x <= ub

instance Transformable (AffineTransform Int) Int where
    applyTransform (AffineTransform a b) x = a * x + b
instance Transformable (BoundedAffineTransform Int) Int where
    applyTransform (BoundedAffineTransform domain affineTransform) x
      | member x domain = applyTransform affineTransform x
      | otherwise = x
instance Transformable (SeedTransform Int) Int where
    applyTransform (SeedTransform _ transforms) x = case transform of Nothing -> x; Just t -> applyTransform t x
      where transform = find (member x . domain) transforms

parseSeeds :: [Char] -> [Int]
parseSeeds('s':'e':'e':'d':'s':':':as) = Prelude.map read $ words as :: [Int]

parseMap :: [Char] -> SeedTransform Int
parseMap paragraph = SeedTransform name (map parseTransform mapStrings)
  where tmp = splitOn (pack " map:\n") (pack paragraph)
        name = unpack (head tmp)
        mapStrings = lines (unpack (last tmp))
        valuesToTransform [a, b, c] = BoundedAffineTransform (Range b (b+c-1)) (AffineTransform 1 (a-b))
        parseTransform mapStr = valuesToTransform (take 3 (Prelude.map read $ words mapStr :: [Int]))

solve1 :: (Foldable t, Transformable a1 a2, Ord a2) => t a1 -> [a2] -> Maybe a2
solve1 seedMaps seeds = case map (tmp seedMaps) seeds of [] -> Nothing; ls -> Just (minimum ls)
  where tmp seedMaps seed = foldl (flip applyTransform) seed seedMaps

parseSeeds2 :: [Char] -> [Int]
parseSeeds2 as = helper (parseSeeds as)
  where helper [] = []
        helper (a:b:bs) = [a..(a+b-1)] ++ helper bs

main :: IO ()
main = do
    input <- readFile "data/input1.txt"
    let paragraphs = map unpack (splitOn (pack "\n\n") (pack input))

    let seeds = parseSeeds (head paragraphs)
    let seedMaps = map parseMap (drop 1 paragraphs)
    print ""
    print (solve1 seedMaps seeds)

    let seeds2 = parseSeeds2 (head paragraphs)
    let seedMaps = map parseMap (drop 1 paragraphs)
    print ""
    print (solve1 seedMaps seeds2)
