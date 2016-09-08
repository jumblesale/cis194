skips :: [a] -> [[a]]
skips [] = []
skips x = skips' (length x) x

-- "hello" - length 5
-- skips' 5 "hello"
-- everyN (5 - (5-1)) = everyN 1
-- skips' 4 "hello"
-- everyN (5 - (4-1)) = everyN 2
-- skips' 3 "hello"
-- everyN (5 - (3-1)) = everyN 3
-- ...
skips' :: Int -> [a] -> [[a]]
skips' 0 x = []
skips' n x = everyN ((length x) - (n-1)) x : skips' (n-1) x

everyN :: Int -> [a] -> [a]
everyN _ [] = []
everyN n x
    | length x < n = []
    | otherwise = (x !! (n-1)) : everyN n (drop n x)
