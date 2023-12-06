module Interval where

data Interval a = Range { lower :: a, upper :: a}
                | Empty
                | Full
  deriving (Show, Eq)

-- Smart Range constructor.
makeRange :: Ord a => a -> a -> Interval a
makeRange lb ub = if lb <= ub then Range lb ub else Empty

-- Check if a number is in a Range.
member :: Ord a => a -> Interval a -> Bool
member a (Range lb ub) = lb <= a && a <= ub
member _ Empty = False
member _ Full = True

-- Intersection between two Intervals.
intersection :: Ord a => Interval a -> Interval a -> Interval a
intersection Empty _ = Empty
intersection _ Empty = Empty
intersection Full i = i
intersection i Full = i
intersection (Range l1 u1) (Range l2 u2)
    | u1 < l2 || u2 < l1 = Empty                    -- No overlap
    | otherwise = Range (max l1 l2) (min u1 u2)     -- Overlapping intervals

data IntervalUnion a = Overlapping (Interval a)
                     | Disjoint (Interval a) (Interval a)
-- Union of intersections.
union :: Ord a => Interval a -> Interval a -> IntervalUnion a
union Empty a = Overlapping a
union a Empty = Overlapping a
union Full i = Overlapping Full
union i Full = Overlapping Full
union (Range l1 u1) (Range l2 u2)
    | u1 < l2 || u2 < l1 = Disjoint (Range l1 u1) (Range l2 u2)   -- No overlap
    | otherwise = Overlapping (Range (min l1 l2) (max u1 u2))     -- Overlapping intervals

data IntervalDifference a = Overlapping (Interval a)
                          | Disjoint (Interval a) (Interval a)
difference :: Ord a => Interval a -> Interval a -> Interval a
difference Empty a = Empty
difference a Empty = a
difference Full i = Overlapping Full
difference i Full = Overlapping Full
