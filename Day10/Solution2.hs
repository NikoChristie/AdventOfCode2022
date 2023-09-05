import System.IO
import Data.List

data Instruction = NOOP | ADDX Int deriving (Show)

-- 20th, 60th, 100th, 140th, 180th

indices = [20, 60, 100, 140, 180, 220]

main = do

	handle <- openFile "Input.txt" ReadMode
	contents <- hGetContents handle

	let instructions = map (readInstruction) $ lines contents
	    history      = [1] ++ mapInstructions instructions
	    solution     = map (sum) $ map (\x -> take x history) indices
	
	print $ instructions
	print $ history
	print $ solution
	print $ (sum . map (\(a, b) -> a * b)) $ zip solution indices

        hClose handle   	


readInstruction :: String -> Instruction
readInstruction xs
	| "addx " `isInfixOf` xs = ADDX (read $ drop 5 xs)
	| xs == "noop"           = NOOP
        | otherwise              = undefined

mapInstructions :: [Instruction] -> [Int]
mapInstructions [] = []
mapInstructions (NOOP:ns) = [0] ++ mapInstructions ns
mapInstructions (ADDX i:ns) = [0, i] ++ mapInstructions ns

instructionToValue :: Instruction -> Int
instructionToValue (NOOP)   = 0
instructionToValue (ADDX i) = i
