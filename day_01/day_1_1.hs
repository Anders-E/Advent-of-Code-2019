main :: IO ()
main = interact (show . sum . map fuelReq . map read . lines)

fuelReq :: Integer -> Integer
fuelReq m = m `div` 3 - 2
