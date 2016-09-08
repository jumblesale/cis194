histogram :: [Int] -> String
histogram x = histogram' (maximum counts) counts
    where counts = countNumbers x

countNumbers x = map (\n -> length $ filter (== n) x) [0..9]

histogram' :: Int -> [Int] -> String
histogram' 0 _ = drawAxis
histogram' rowNumber h = map (\x -> if x >= rowNumber then '*' else ' ') h ++
    "\n" ++
    histogram' (rowNumber - 1) h

drawAxis = "==========\n0123456789\n"
