main = print . length . filter twoAdjacentsSame . filter neverDecrease $ map intToList [254032..789860]

twoAdjacentsSame xs = 2 `elem` [length $ filter (== x) xs | x <- [0..9]]

neverDecrease xs = notElem False $ zipWith (<=) xs (tail xs)

intToList x = intToList' x []
intToList' 0 xs = xs
intToList' x xs = intToList' (x `div` 10) (x `mod` 10 : xs)
