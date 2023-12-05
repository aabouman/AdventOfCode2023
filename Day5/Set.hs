module Set where

import Data.List (sortOn)

import qualified Interval as I

data Set = IntervalSet [I.Interval]
         | Empty
         | Full
  deriving Show

setElem a Empty = False
setElem a Full = True
setElem a Full = True

-- Join intervals which overlap.
compactify :: Set -> Set
compactify (IntervalSet is)
  | I.Full `elem` is = Full
  | otherwise = IntervalSet (mergeIn (sortOn I.lower (filter (/= I.Empty) is)))
  where mergeIn [] = []
        mergeIn [a] = [a]
        mergeIn (r1:r2:i1) = case r1 I.\/ r2 of
          I.Disjoint _ _ -> r1:mergeIn (r2:i1)
          I.Overlapping r -> mergeIn (r:i1)
compactify a = a

-- Union of Sets.
(\/) :: Set -> Set -> Set
Empty \/ a = a
a \/ Empty = a
Full \/ a = Full
a \/ Full = Full
IntervalSet i1 \/ IntervalSet i2 = compactify (IntervalSet (i1 ++ i2))

-- Intersection of Set.
(/\) :: Set -> Set -> Set
Empty /\ a = Empty
a /\ Empty = Empty
Full /\ a = a
a /\ Full = a
is /\ (IntervalSet i2) = foldr (\/) Empty (intersectionSets is)
  where intersectSetInterval :: Set -> I.Interval -> Set
        intersectSetInterval (IntervalSet i1) r2 = compactify (IntervalSet (map (I./\ r2) i1))
        intersectionSets is = map (intersectSetInterval is) i2


