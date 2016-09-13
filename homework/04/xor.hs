xor :: [Bool] -> Bool
xor = odd . length . filter (==True)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> [(f x)] ++ acc) []
