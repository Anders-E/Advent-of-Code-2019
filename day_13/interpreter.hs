import Data.List.Split
import qualified Data.Vector as V

main = do
    prog <- readFile "input.txt"
    res <- run . V.map read . V.fromList $ splitOn "," prog
    return ()

-- RUN PROGRAM
run :: V.Vector Integer -> IO (V.Vector Integer)
run prog = run' (prog V.++ extendedMemory) 0 0
  where
    extendedMemory = V.fromList $ replicate 1000 0
run' :: V.Vector Integer -> Int -> Int -> IO (V.Vector Integer)
run' prog i relBase
    | prog V.! i == 99 = return prog
    | otherwise = do
        (prog', i', relBase') <- execInstruction prog opcode i relBase
        run' prog' i' relBase'
  where
    opcode = parseOpcode $ prog V.! i

execInstruction :: V.Vector Integer -> (Integer, Integer, Integer, Integer) -> Int -> Int -> IO ((V.Vector Integer, Int, Int))
-- Addition
execInstruction prog (1, a, b, c) i relBase = return (prog V.// [(out, res)], i + 4, relBase)
  where
    (x, y, out) = params (a, b, c) prog i relBase
    res = x + y

-- Multiplication
execInstruction prog (2, a, b, c) i relBase = return (prog V.// [(out, res)], i + 4, relBase)
  where
    (x, y, out) = params (a, b, c) prog i relBase
    res = x * y

-- Input
execInstruction prog (3, a, b, c) i relBase =
  do
    putStrLn "Enter Integer:"
    res <- getLine
    return (prog V.// [(out, read res)], i + 2, relBase)
  where
    out = outParam a (i + 1) prog relBase

-- Output
execInstruction prog (4, a, b, c) i relBase =
  do
    print $ x
    return (prog, i + 2, relBase)
  where
    (x, _, _) = params (a, b, c) prog i relBase

-- jump-if-true
execInstruction prog (5, a, b, c) i relBase
    | x /= 0 = return (prog, fromIntegral y, relBase)
    | otherwise = return (prog, i + 3, relBase)
  where
    (x, y, _) = params (a, b, c) prog i relBase

-- jump-if-false
execInstruction prog (6, a, b, c) i relBase
    | x == 0 = return (prog, fromIntegral y, relBase)
    | otherwise = return (prog, i + 3, relBase)
  where
    (x, y, _) = params (a, b, c) prog i relBase

-- less than
execInstruction prog (7, a, b, c) i relBase
    | x < y = return (prog V.// [(out, 1)], i + 4, relBase)
    | otherwise = return (prog V.// [(out, 0)], i + 4, relBase)
  where
    (x, y, out) = params (a, b, c) prog i relBase

-- equals
execInstruction prog (8, a, b, c) i relBase
    | x == y = return (prog V.// [(out, 1)], i + 4, relBase)
    | otherwise = return (prog V.// [(out, 0)], i + 4, relBase)
  where
    (x, y, out) = params (a, b, c) prog i relBase

-- modify relative base
execInstruction prog (9, a, b, c) i relBase = return (prog, i + 2, relBase')
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
