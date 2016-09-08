-- [4,2,5,3,6,2,4]
-- [4,2,5]
-- 2 > 4 && 4 > 5? False
-- advance 1
-- [2,5,3,6,2,4]
-- [2,5,3]
-- 5 > 2 && 5 > 3? True
-- advance 2
-- [3,6,2,4]
-- ...
maxima (x:y:z:zs)
    | localMax x y z = y : maxima (z:zs)
    | otherwise = maxima (y:z:zs)
    where localMax x y z = (y > x && y > z)
maxima _ = []
