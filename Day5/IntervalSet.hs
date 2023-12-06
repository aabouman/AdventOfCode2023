module IntervalSet where

import Data.List (sortOn)

import qualified Interval as I

data IntervalSet a = IntervalSet [I.Interval a]
                   | Empty
                   | Full
  deriving Show
fromList :: Ord a => [I.Interval a] -> IntervalSet a
fromList is = compactify (IntervalSet is)

member :: Ord a => a -> IntervalSet a -> Bool
member a Empty = False
member a Full = True
member a (IntervalSet is) = any (I.member a) is

-- Join intervals which overlap.
compactify :: Ord a => IntervalSet a -> IntervalSet a
compactify (IntervalSet is)
  | I.Full `elem` is = Full
  | otherwise = IntervalSet (mergeIn (sortOn I.lower (filter (/= I.Empty) is)))
  where mergeIn [] = []
        mergeIn [a] = [a]
        mergeIn (r1:r2:i1) = case I.union r1 r2 of
          I.Disjoint _ _ -> r1:mergeIn (r2:i1)
          I.Overlapping r -> mergeIn (r:i1)
compactify a = a

-- Union of Sets.
union :: Ord a => IntervalSet a -> IntervalSet a -> IntervalSet a
union Empty a = a
union a Empty = a
union Full a = Full
union a Full = Full
union (IntervalSet i1) (IntervalSet i2) = compactify (IntervalSet (i1 ++ i2))

-- Intersection of Set.
intersection :: Ord a => IntervalSet a -> IntervalSet a -> IntervalSet a
intersection Empty a = Empty
intersection a Empty = Empty
intersection Full a = a
intersection a Full = a
intersection is (IntervalSet i2) = compactify (foldr union Empty (intersectionSets is))
  where intersectSetInterval (IntervalSet i1) r2 = compactify (IntervalSet (map (I.intersection r2) i1))
        intersectionSets is = map (intersectSetInterval is) i2


