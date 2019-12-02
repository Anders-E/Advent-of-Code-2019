import Data.List.Split
import Data.Vector as V

main :: IO ()
main = interact (show . nounVerb . V.map read . fromList . splitOn ",")

nounVerb :: Vector Int -> Int
nounVerb prog = Prelude.head [100 * n + v | n <- [1..99], v <- [1..99], run prog n v == 19690720]

run :: Vector Int -> Int -> Int -> Int
run prog n v = V.head $ run' (prog // [(1, n), (2, v)]) 0
run' :: Vector Int -> Int -> Vector Int
run' prog i
    | opc == 99 = prog
    | otherwise = run' (prog // [(out, op prog opc a b)]) (i + 4)
  where
    opc = prog ! i
    a   = prog ! (i + 1)
    b   = prog ! (i + 2)
    out = prog ! (i + 3)

op :: Vector Int -> Int -> Int -> Int -> Int
op prog 1 a b = (prog ! a) + (prog ! b)
op prog 2 a b = (prog ! a) * (prog ! b)
