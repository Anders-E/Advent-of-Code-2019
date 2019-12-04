main = print . length . filter twoAdjacentsSame . filter neverDecrease $ map intToList [254032..789860]

twoAdjacentsSame xs = elem True (zipWith (==) xs $ tail xs)

neverDecrease xs = not $ elem False $ zipWith (<=) xs (tail xs)

intToList x = intToList' x []
intToList' 0 xs = xs
intToList' x xs = intToList' (x `div` 10) (x `mod` 10 : xs)
