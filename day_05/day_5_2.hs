import Data.List.Split
import Data.Vector as V

main = do
    prog <- readFile "input.txt"
    res <- run . V.map read . fromList $ splitOn "," prog
    return ()

-- RUN PROGRAM
run :: Vector Int -> IO (Vector Int)
run prog = run' prog 0
run' :: Vector Int -> Int -> IO (Vector Int)
run' prog i
    | prog ! i == 99 = return prog
    | otherwise = do
        (prog', i') <- execInstruction prog opcode i
        run' prog' i'
  where
    opcode = parseOpcode $ prog ! i

execInstruction :: Vector Int -> Opcode -> Int -> IO ((Vector Int, Int))
-- Addition
execInstruction prog (Opcode 1 a b c) i = return (prog // [(outPos, res)], i + 4)
  where
    x = param (immediate a) (prog ! (i + 1)) prog
    y = param (immediate b) (prog ! (i + 2)) prog
    res = x + y
    outPos = out (immediate c) (i + 3) prog

-- Multiplication
execInstruction prog (Opcode 2 a b c) i = return (prog // [(outPos, res)], i + 4)
  where
    x = param (immediate a) (prog ! (i + 1)) prog
    y = param (immediate b) (prog ! (i + 2)) prog
    res = x * y
    outPos = out (immediate c) (i + 3) prog

-- Input
execInstruction prog (Opcode 3 a b c) i =
  do
    putStrLn "Enter integer:"
    res <- getLine
    return (prog // [(outPos, read res)], i + 2)
  where
    outPos = out (immediate c) (i + 1) prog

-- Output
execInstruction prog (Opcode 4 a b c) i =
  do
    print $ x
    return (prog, i + 2)
  where
    x = param (immediate a) (prog ! (i + 1)) prog

-- jump-if-true
execInstruction prog (Opcode 5 a b c) i
    | x /= 0 = return (prog, y)
    | otherwise = return (prog, i + 3)
  where
    x = param (immediate a) (prog ! (i + 1)) prog
    y = param (immediate b) (prog ! (i + 2)) prog

-- jump-if-false
execInstruction prog (Opcode 6 a b c) i
    | x == 0 = return (prog, y)
    | otherwise = return (prog, i + 3)
  where
    x = param (immediate a) (prog ! (i + 1)) prog
    y = param (immediate b) (prog ! (i + 2)) prog

-- less than
execInstruction prog (Opcode 7 a b c) i
    | x < y = return (prog // [(outPos, 1)], i + 4)
    | otherwise = return (prog // [(outPos, 0)], i + 4)
  where
    x = param (immediate a) (prog ! (i + 1)) prog
    y = param (immediate b) (prog ! (i + 2)) prog
    outPos = out (immediate c) (i + 3) prog

-- equals
execInstruction prog (Opcode 8 a b c) i
    | x == y = return (prog // [(outPos, 1)], i + 4)
    | otherwise = return (prog // [(outPos, 0)], i + 4)
  where
    x = param (immediate a) (prog ! (i + 1)) prog
    y = param (immediate b) (prog ! (i + 2)) prog
    outPos = out (immediate c) (i + 3) prog


-- True = Immediate
-- False = Position
param :: Bool -> Int -> Vector Int -> Int
param True p _ = p
param False p prog = prog ! p

out :: Bool -> Int -> Vector Int -> Int
out True p _ = p
out False p prog = prog ! p

-- (operation, 1st param mode, 2nd param mode, 3rd param mode)
data Opcode = Opcode Int Int Int Int deriving (Show)

parseOpcode :: Int -> Opcode
parseOpcode opcode = Opcode op a b c
  where
    op = opcode `mod` 100
    a = (opcode `div` 100) `mod` 10
    b = (opcode `div` 1000) `mod` 10
    c = opcode `div` 10000

immediate 0 = False
immediate 1 = True
