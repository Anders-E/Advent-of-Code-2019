import Data.List.Split
import Data.Vector as V

main = interact (show . nounVerb . V.map read . fromList . splitOn ",")

nounVerb prog = Prelude.head [100 * n + v | n <- [1..99], v <- [1..99], run prog n v == 19690720]

run prog n v = V.head $ run' (prog // [(1, n), (2, v)]) 0
run' prog i
    | opc == 99 = prog
    | otherwise = run' (prog // [(out, res)]) (i + 4)
  where
    opc = prog ! i
    a   = prog ! (prog ! (i + 1))
    b   = prog ! (prog ! (i + 2))
    out = prog ! (i + 3)
    fun = op opc
    res = fun a b

op 1 = (+)
op 2 = (*)
