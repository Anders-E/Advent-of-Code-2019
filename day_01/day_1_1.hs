main = interact (show . sum . map fuelReq . map read . lines)

fuelReq m = m `div` 3 - 2
