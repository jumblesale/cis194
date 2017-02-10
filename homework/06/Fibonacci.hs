module Fibonnaci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map (fib) [0..]

fibs2 = fib' 0 1 where
    fib' x y = x : fib' y (x + y) -- I would never have figured this out on my own
                                  -- but it is really cool

data Stream a = Stream a (Stream a)

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamToList :: Stream a -> [a]
streamToList (Stream x xs) = x : streamToList xs

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x xs) = Stream (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = Stream s (streamFromSeed f (f s))

nats :: Stream Integer
nats = streamFromSeed (+1) 1

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x xs) ys = Stream x (interleaveStreams ys xs)

ruler :: Stream Integer
ruler = foldr1 interleaveStreams (map streamRepeat [0..])
