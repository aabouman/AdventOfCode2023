module Interval where

data Interval = Range { lower :: Int, upper :: Int}
              | Empty
              | Full
  deriving (Show, Eq)

-- Smart Range constructor.
makeRange :: Int -> Int -> Interval
makeRange lb ub = if lb <= ub then Range lb ub else Empty

-- Check if a number is in a Range.
inRange :: Int -> Interval -> Bool
inRange a (Range lb ub) = lb <= a && a <= ub
inRange _ Empty = False
inRange _ Full = True

-- Compute the size of the Intervals.
size :: Interval -> Int
size (Range lb ub) = (ub - lb) + 1
size Empty = 0
size Full = 0

-- Intersection between two Intervals.
(/\) :: Interval -> Interval -> Interval
Empty /\ _ = Empty
_ /\ Empty = Empty
Full /\ i = i
i /\ Full = i
Range l1 u1 /\ Range l2 u2
    | u1 < l2 || u2 < l1 = Empty                    -- No overlap
    | otherwise = Range (max l1 l2) (min u1 u2)     -- Overlapping intervals

data IntervalUnion = Overlapping Interval
                   | Disjoint Interval Interval
-- Union of intersections.
(\/) :: Interval -> Interval -> IntervalUnion
Empty \/ a = Overlapping a
a \/ Empty = Overlapping a
Full \/ i = Overlapping Full
i \/ Full = Overlapping Full
Range l1 u1 \/ Range l2 u2
    | u1 < l2 || u2 < l1 = Disjoint (Range l1 u1) (Range l2 u2)   -- No overlap
    | otherwise = Overlapping (Range (min l1 l2) (max u1 u2))     -- Overlapping intervals

