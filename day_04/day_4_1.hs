main = print . length . filter twoAdjacentsSame . filter neverDecrease $ map intToList [254032..789860]

twoAdjacentsSame xs = not . null $ zipWith (==) xs (tail xs)

neverDecrease xs = null $  zipWith (>) xs (tail xs)

intToList x = intToList' x []
intToList' 0 xs = xs
intToList' x xs = intToList' (x `div` 10) (x `mod` 10 : xs)
