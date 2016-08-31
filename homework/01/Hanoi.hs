module Hanoi where

type Peg = String
type Move = (Peg, Peg)

-- solve a hanoi pronlem
-- example:
-- hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n from to with
    | n >=1 = (hanoi (n-1) from with to) ++ [(from, to)] ++ (hanoi (n-1) with to from)
    | otherwise = []
