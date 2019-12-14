import Data.List.Split
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

main = do
    prog <- readFile "input.txt"
    let (_, _, output, _, _, _) = run [] . V.map read . V.fromList $ splitOn "," prog
    let blocks = M.size $ M.filter (==2) $ game output
    print blocks

-- GAME
game :: [Integer] -> M.Map (Integer, Integer) Integer
game output = foldl (\m [x,y,id] -> M.insert (x, y) id m) M.empty (chunksOf 3 output)

-- RUN PROGRAM
run :: [Integer] -> V.Vector Integer -> (Bool, [Integer], [Integer], V.Vector Integer, Int, Int)
run input prog = run' input [] (prog V.++ extendedMemory) 0 0
  where
    extendedMemory = V.fromList $ replicate 1000 0
run' :: [Integer] -> [Integer] -> V.Vector Integer -> Int -> Int -> (Bool, [Integer], [Integer], V.Vector Integer, Int, Int)
run' input output prog i relBase
    | prog V.! i == 99 = (True, input, output, prog, i, relBase)
    | otherwise = let (prog', i', relBase', input', output') = execInstruction prog opcode i relBase input output in
        run' input' output' prog' i' relBase'
  where
    opcode = parseOpcode $ prog V.! i

execInstruction :: V.Vector Integer -> (Integer, Integer, Integer, Integer) -> Int -> Int -> [Integer] -> [Integer] -> (V.Vector Integer, Int, Int, [Integer], [Integer])
-- Addition
execInstruction prog (1, a, b, c) i relBase input output = (prog V.// [(out, res)], i + 4, relBase, input, output)
  where
    (x, y, out) = params (a, b, c) prog i relBase
    res = x + y

-- Multiplication
execInstruction prog (2, a, b, c) i relBase input output = (prog V.// [(out, res)], i + 4, relBase, input, output)
  where
    (x, y, out) = params (a, b, c) prog i relBase
    res = x * y

-- Input
execInstruction prog (3, a, b, c) i relBase (res:is) output = (prog V.// [(out, res)], i + 2, relBase, is, output)
  where
    out = outParam a (i + 1) prog relBase

-- Output
execInstruction prog (4, a, b, c) i relBase input output = (prog, i + 2, relBase, input, output ++ [x])
  where
    (x, _, _) = params (a, b, c) prog i relBase

-- jump-if-true
execInstruction prog (5, a, b, c) i relBase input output
    | x /= 0 = (prog, fromIntegral y, relBase, input, output)
    | otherwise = (prog, i + 3, relBase, input, output)
  where
    (x, y, _) = params (a, b, c) prog i relBase

-- jump-if-false
execInstruction prog (6, a, b, c) i relBase input output
    | x == 0 = (prog, fromIntegral y, relBase, input, output)
    | otherwise = (prog, i + 3, relBase, input, output)
  where
    (x, y, _) = params (a, b, c) prog i relBase

-- less than
execInstruction prog (7, a, b, c) i relBase input output
    | x < y = (prog V.// [(out, 1)], i + 4, relBase, input, output)
    | otherwise = (prog V.// [(out, 0)], i + 4, relBase, input, output)
  where
    (x, y, out) = params (a, b, c) prog i relBase

-- equals
execInstruction prog (8, a, b, c) i relBase input output
    | x == y = (prog V.// [(out, 1)], i + 4, relBase, input, output)
    | otherwise = (prog V.// [(out, 0)], i + 4, relBase, input, output)
  where
    (x, y, out) = params (a, b, c) prog i relBase

-- modify relative base
execInstruction prog (9, a, b, c) i relBase input output = (prog, i + 2, relBase', input, output)
  where
    (x, _, _) = params (a, b, c) prog i relBase
    relBase' = fromIntegral x + relBase

params :: (Integer, Integer, Integer) -> V.Vector Integer -> Int -> Int -> (Integer, Integer, Int)
params (a, b, c) prog i relBase = (
    param prog a (i + 1) relBase,
    param prog b (i + 2) relBase,
    outParam c (i + 3) prog relBase
  )

param :: V.Vector Integer -> Integer -> Int -> Int -> Integer
-- Position
param prog 0 i _ = prog V.! (fromIntegral $ prog V.! i)
-- Immediate
param prog 1 i _ = prog V.! i
-- Relative
param prog 2 i relBase = prog V.! (relBase + (fromIntegral $ prog V.! i))

outParam :: Integer -> Int -> V.Vector Integer -> Int -> Int
outParam 0 p prog relBase = fromIntegral $ prog V.! p
outParam 1 p _ relBase = p
outParam 2 p prog relBase = (fromIntegral $ prog V.! p) + fromIntegral relBase

parseOpcode :: Integer -> (Integer, Integer, Integer, Integer)
parseOpcode opcode = (op, a, b, c)
  where
    op = opcode `mod` 100
    a = (opcode `div` 100) `mod` 10
    b = (opcode `div` 1000) `mod` 10
    c = opcode `div` 10000
