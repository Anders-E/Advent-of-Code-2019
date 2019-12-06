import Data.List.Split
import Data.Vector as V

main = interact ((Prelude.++ "\n") . show . run . V.map read . fromList . splitOn ",")

-- RUN PROGRAM

run :: Vector Int -> Vector Int
run prog = run' prog 0
run' :: Vector Int -> Int -> Vector Int
run' prog i
    | prog ! i == 99 = prog
    | otherwise = run' prog' i'
  where
    res = execInstruction prog opcode i
    prog' = fst res
    i' = snd res
    opcode = parseOpcode $ prog ! i

execInstruction :: Vector Int -> Opcode -> Int -> (Vector Int, Int)
-- Addition
execInstruction prog (Opcode 1 a b c) i = (prog // [(outPos, res)], i + 4)
  where
    x = param (immediate a) (prog ! (i + 1)) prog
    y = param (immediate b) (prog ! (i + 2)) prog
    res = x + y
    outPos = out (immediate c) (i + 3) prog

-- Multiplication
execInstruction prog (Opcode 2 a b c) i = (prog // [(outPos, res)], i + 4)
  where
    x = param (immediate a) (prog ! (i + 1)) prog
    y = param (immediate b) (prog ! (i + 2)) prog
    res = x * y
    outPos = out (immediate c) (i + 3) prog

-- True = Immediate
-- False = Position
param :: Bool -> Int -> Vector Int -> Int
param True p _ = p
param False p prog = prog ! p

out True p _ = p
out False p prog = prog ! p

-- (operation, 1st param mode, 2nd param mode, 3rd param mode)
data Opcode = Opcode Int Int Int Int deriving (Show)

parseOpcode :: Int -> Opcode
parseOpcode opcode = Opcode op a b c
  where
    op = opcode `mod` 100
    a = opcode `div` 10000
    b = (opcode `div` 1000) `mod` 10
    c = (opcode `div` 100) `mod` 10

immediate 0 = False
immediate 1 = True
