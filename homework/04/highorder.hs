module Highorder where

import Data.List

data Tree a = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
     | even x    = (x - 2) * fun1 xs
     | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter (even)

-- 3
-- f(3 * 3 + 1) = f(10)
-- 10
-- 10 + f(5)
-- 10 + f(3 * 5 + 1) = f(16)
-- 10 + 16 + f(8)
-- 10 + 16 + 8 + f(4)
-- 10 + 16 + 8 + 4 + f(2)
-- 10 + 16 + 8 + 4 + 2
-- 40
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

-- fun2' :: Integer -> Integer

-- iterate (\n -> if even n then div n 2 else 3 * n + 1) 3
-- (take 8 to see all unique entries)
-- [3,10,5,16,8,4,2,1,...]
-- filter (even)
-- [10,16,8,4,2]
-- sum = 40
fun2' =
    sum .
    filter (even) .
    takeWhile (/= 1) .
    iterate (\n -> if even n then div n 2 else 3 * n + 1)
