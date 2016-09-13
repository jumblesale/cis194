module Sieve where

import Data.List

sundaram :: Int -> [Int]
sundaram n = takeWhile (<= n) .           -- under numbers less than n
             map (+1) . map (*2) $        -- (*2) + 1 of the result
             [1..n] \\ (                  -- list difference of first n natural numbers
             map (\(x, y) -> sieve x y) $ -- do the sieve function
             cartProd [1..n] [1..n])      -- cartesian product of numbers between 1 and n

sieve :: Int -> Int -> Int
sieve i j = (i + j + (2 * i * j))

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
