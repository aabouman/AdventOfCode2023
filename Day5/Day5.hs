module Main where

import Data.List
import qualified Interval as I
import qualified Set as S

data Map = Map {from :: Set, to :: Set}
  deriving Show
-- Map a value through a Map.
mapThrough :: Int -> Map -> Int
mapThrough a (Map r1 r2) = if inRange a r1 then lower r2 + (a - lower r1) else a

instance Semigroup Map where
  (Map from1 to1) <> (Map from2 to2) =

main :: IO ()
main = do
    let i1 = I.Range 1 3
    let i2 = I.Range 5 8
    let i3 = I.Range 10 21
    let s1 = S.IntervalSet [ i2, i3, i1 ]

    print (filter (== I.Empty) [ i2, i3, i1 ])

    print (s1)
    print ((S.\/) s1 (S.IntervalSet [I.Range 3 5]))
    print ((S.\/) s1 (S.IntervalSet [I.Range 2 11]))
    print ((S.\/) s1 (S.IntervalSet [I.Full]))
    print ((S./\) s1 (S.IntervalSet [I.Range 3 5]))
