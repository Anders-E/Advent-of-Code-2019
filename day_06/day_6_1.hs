import qualified Data.Map.Strict as Map
import Data.List.Split

main = interact ((++ "\n") . show . orbits . orbitMap . map (splitOn ")") . lines )

orbitMap xs = orbitMap' xs Map.empty
orbitMap' [] m = m
orbitMap' ([k, v]:xs) m = orbitMap' xs (Map.insertWith (++) k [v] m)

orbits m = orbits' m 0 "COM"
orbits' m n k = n + (sum $ map (orbits' m $ n + 1) (Map.findWithDefault [] k m))
