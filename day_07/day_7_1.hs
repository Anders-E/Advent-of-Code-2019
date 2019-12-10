import Data.List
import Data.List.Split
import Data.Vector as V

main = do
    prog <- readFile "input.txt"
    let prog' = V.map (read :: String -> Int) . fromList $ splitOn "," prog
    let res = [(runWithPhases prog' phaseSettings, phaseSettings) | phaseSettings <- permutations [0, 1, 2, 3, 4]]
    print $ fst $ Prelude.maximum res

runWithPhases prog phaseSettings = Data.List.foldr (runWithPhase prog) 0 phaseSettings
runWithPhase prog phaseSetting lastOutput  = run prog [phaseSetting, lastOutput]

-- RUN PROGRAM
run :: Vector Int -> [Int] -> Int
run prog input = run' input [] prog 0
run' :: [Int] -> [Int] -> Vector Int -> Int -> Int
run' input output prog i
    | prog ! i == 99 = Prelude.head output
    | otherwise = let (prog', i', input', output') = execInstruction input output prog opcode i in        
        run' input' output' prog' i'
  where
    opcode = parseOpcode $ prog ! i

execInstruction :: [Int] -> [Int] -> Vector Int -> Opcode -> Int -> (Vector Int, Int, [Int], [Int])
-- Addition
execInstruction input output prog (Opcode 1 a b c) i = (prog // [(outPos, res)], i + 4, input, output)
  where
    x = param (immediate a) (prog ! (i + 1)) prog
    y = param (immediate b) (prog ! (i + 2)) prog
    res = x + y
    outPos = out (immediate c) (i + 3) prog

-- Multiplication
execInstruction input output prog (Opcode 2 a b c) i = (prog // [(outPos, res)], i + 4, input, output)
  where
    x = param (immediate a) (prog ! (i + 1)) prog
    y = param (immediate b) (prog ! (i + 2)) prog
    res = x * y
    outPos = out (immediate c) (i + 3) prog

-- Input
execInstruction (x:xs) output prog (Opcode 3 a b c) i = (prog // [(outPos, x)], i + 2, xs, output)
  where
    outPos = out (immediate c) (i + 1) prog

-- Output
execInstruction input output prog (Opcode 4 a b c) i = (prog, i + 2, input, output Prelude.++ [x])
  where
    x = param (immediate a) (prog ! (i + 1)) prog

-- jump-if-true
execInstruction input output prog (Opcode 5 a b c) i
    | x /= 0 = (prog, y, input, output)
    | otherwise = (prog, i + 3, input, output)
  where
    x = param (immediate a) (prog ! (i + 1)) prog
    y = param (immediate b) (prog ! (i + 2)) prog

-- jump-if-false
execInstruction input output prog (Opcode 6 a b c) i
    | x == 0 = (prog, y, input, output)
    | otherwise = (prog, i + 3, input, output)
  where
    x = param (immediate a) (prog ! (i + 1)) prog
    y = param (immediate b) (prog ! (i + 2)) prog

-- less than
execInstruction input output prog (Opcode 7 a b c) i
    | x < y = (prog // [(outPos, 1)], i + 4, input, output)
    | otherwise = (prog // [(outPos, 0)], i + 4, input, output)
  where
    x = param (immediate a) (prog ! (i + 1)) prog
    y = param (immediate b) (prog ! (i + 2)) prog
    outPos = out (immediate c) (i + 3) prog

-- equals
execInstruction input output prog (Opcode 8 a b c) i
    | x == y = (prog // [(outPos, 1)], i + 4, input, output)
    | otherwise = (prog // [(outPos, 0)], i + 4, input, output)
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
