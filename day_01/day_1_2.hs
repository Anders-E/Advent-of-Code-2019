main :: IO ()
main = interact (show . sum . (map fuelReq) . (map read) . lines)

fuelReq :: Integer -> Integer
fuelReq m
    | m < 9 = 0
    | otherwise = fuel + fuelReq fuel
  where
    fuel = m `div` 3 - 2
