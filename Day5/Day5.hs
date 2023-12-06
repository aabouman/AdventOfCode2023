module Main where

import Data.List
import Data.Monoid

import qualified Interval as I
import qualified IntervalSet as S


data Map = IntervalMap {from :: S.Interval Int, to :: S.Interval Int}
         | Map [IntervalMap]
  deriving Show
-- -- Map a value through a Map.
mapValue :: Int -> Map -> Int
mapValue a (IntervalMap from to) = if S.member a from then lower r2 + (a - lower r1) else a
-- mapValue a (Map ims) = if S.member a from then lower r2 + (a - lower r1) else a

mapInterval :: S.Interval Int -> Map -> [S.Interval Int]
mapInterval (Range lb up) (IntervalMap from to) =
  where intersection (Range lb up)
mapInterval i (IntervalMap from to) = [i]

check (IntervalMap {}) = True
check (Map (im:ims)) = foldr (\x -> I.intersection (from x)) (from im) im

instance Semigroup Map where
  (IntervalMap from1 to1) <> (IntervalMap from2 to2) =
    where outInIntersection = case (intersection to1 from2) of Empty -> IntervalMap Empty Empty

instance Monoid Map where
    mempty = IntervalMap Full Full -- Identity Map

main :: IO ()
main = do
    let i1 = I.Range 1 3
    let i2 = I.Range 4 8
    let i3 = I.Range 10 21
    let s1 = S.fromList [ i2, i3, i1 ]

    print (filter (== I.Empty) [ i2, i3, i1 ])

    print (s1)
    print ((S.union) s1 (S.IntervalSet [I.Range 3 5]))
    print ((S.union) s1 (S.IntervalSet [I.Range 2 11]))
    print ((S.union) s1 (S.IntervalSet [I.Full]))
    print ((S.intersection) s1 (S.IntervalSet [I.Range 3 5]))
