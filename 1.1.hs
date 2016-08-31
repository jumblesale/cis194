toDigits :: Integer -> [Integer]
toDigits n
    | n < 1 = []
    | otherwise = map (\x -> read [x] :: Integer) (show n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther n = zipWith (*) n (cycle [1, 2])

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

validate Integer -> Bool
validate x = mod (sumDigits . doubleEveryOther . reverse . toDigits $ x) 10 == 0
