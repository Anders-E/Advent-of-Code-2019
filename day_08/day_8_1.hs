import Data.List
import Data.List.Split
import Data.Char

main = interact ((++ "\n") . show . checksum . map digitToInt)

checksum xs = (count 1 targetLayer) * (count 2 targetLayer)
  where
    layers = chunksOf (25 * 6) xs
    zeroCounts = map (count 0) layers
    minZerosIndex = head $ elemIndices (minimum zeroCounts) zeroCounts
    targetLayer = layers !! minZerosIndex

count x xs = length $ filter (==x) xs
