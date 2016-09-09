histogram :: [Int] -> String
histogram x = histogram' (maximum counts) counts
    where counts = countNumbers x

countNumbers xs = map (\n -> (foldr (\x acc -> if x == n then acc + 1 else acc) 0 xs)) [0..9]

histogram' :: Int -> [Int] -> String
histogram' 0 _ = drawAxis
histogram' rowNumber h = map (\x -> if x >= rowNumber then '*' else ' ') h ++
    "\n" ++
    histogram' (rowNumber - 1) h

drawAxis = "==========\n0123456789\n"
