import qualified Data.Set as S

main = interact ((++ "\n") . show . maximum . monitoringStations . readAsteroids)

monitoringStations asteroids = map (monitoringStation asteroids) asteroids
monitoringStation asteroids origin  = length dirs
  where
    dirs = S.fromList . map direction $ map (flip subTup origin) (filter (/= origin) asteroids)

-- Vectors
subTup (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
direction (x, y) = (x `div` (gcd x y), y `div` (gcd x y))

-- Input
width = 33
coords = zip (cycle [0..width-1]) $ map (flip div width) [0..]
readAsteroids s = map fst . filter (\(_, c) -> c /= '.') . zip coords $ filter (/='\n') s
