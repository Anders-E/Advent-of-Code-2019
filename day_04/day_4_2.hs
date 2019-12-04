main = print . length . filter hasDouble . filter sorted $ map intToList [254032..789860]

hasDouble xs = 2 `elem` [length $ filter (== x) xs | x <- [0..9]]

sorted xs = notElem False $ zipWith (<=) xs (tail xs)

intToList x = intToList' x []
intToList' 0 xs = xs
intToList' x xs = intToList' (x `div` 10) (x `mod` 10 : xs)
