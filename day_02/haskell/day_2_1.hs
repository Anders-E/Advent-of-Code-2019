import Data.List.Split
import Data.Vector as V

main = interact (show . run . V.map read . fromList . splitOn ",")

run prog = V.head $ run' (prog // [(1, 12), (2, 2)]) 0
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
