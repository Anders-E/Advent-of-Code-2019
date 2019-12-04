main = print $ length $ filter twoAdjacentsSame $ filter neverDecrease $ map intToList [254032..789860]

twoAdjacentsSame [_] = False
twoAdjacentsSame (x:y:xs)
    | x == y = True
    | otherwise = twoAdjacentsSame (y:xs)

neverDecrease [_] = True
neverDecrease (x:y:xs)
    | x > y = False
    | otherwise = neverDecrease (y:xs)

intToList x = intToList' x []
intToList' 0 xs = xs
intToList' x xs = intToList' (x `div` 10) (x `mod` 10 : xs)
