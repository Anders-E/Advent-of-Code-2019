import qualified Data.Map.Strict as Map
import Data.List.Split

main = interact (show . youSanDist . orbitMap . map (splitOn ")") . lines )

orbitMap xs = orbitMap' xs Map.empty
orbitMap' [] m = m
orbitMap' ([v, k]:xs) m = orbitMap' xs (Map.insert k v m)

pathToCOM m k = pathToCOM' m [] k
pathToCOM' m path [] = path
pathToCOM' m path k = pathToCOM' m (k:path) (Map.findWithDefault [] k m)

youSanDist m = length p1 + length p2 - length common * 2 - 2
  where
    p1 = pathToCOM m "YOU" 
    p2 = pathToCOM m "SAN"
    common = takeWhile (\(x, y) -> x == y) (zip p1 p2)
