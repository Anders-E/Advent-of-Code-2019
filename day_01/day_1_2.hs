main = interact (show . sum . map fuelReq . map read . lines)

fuelReq m
    | m < 9 = 0
    | otherwise = fuel + fuelReq fuel
  where
    fuel = m `div` 3 - 2
