main = print . length . filter hasDuplicate . filter sorted $ map intToList [254032..789860]

hasDuplicate xs = elem True (zipWith (==) xs $ tail xs)

sorted xs = notElem False $ zipWith (<=) xs (tail xs)

intToList x = intToList' x []
intToList' 0 xs = xs
intToList' x xs = intToList' (x `div` 10) (x `mod` 10 : xs)
