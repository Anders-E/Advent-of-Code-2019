import Data.List
import Data.List.Split
import Data.Char

main = interact (unlines . map (map (\c -> if c == 0 then ' ' else '#')) . decode . map digitToInt)

decode xs = chunksOf 25 . map (head . dropWhile (==2)) . transpose $ chunksOf (25 * 6) xs
