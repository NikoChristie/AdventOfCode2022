import System.IO
import Data.List

data Direction = UP | DOWN | LEFT | RIGHT deriving (Show)

contents = "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"

main = do
	--handle <- openFile "Input.txt" ReadMode
	--contents <- hGetContents handle

	--let values = map (\(dir:_:amount) -> (charToDirection dir, read amount)) $ lines contents :: [(Direction, Double)]
	let moves    = concat $ map (\(dir:_:amount) -> take (read amount) (repeat (charToDirection dir))) (lines contents)
            solution = ropeBridge moves (0, 0) (0, 0)

	print $ moves
	print $ solution

        --hClose handle   	

charToDirection :: Char -> Direction
charToDirection 'U' = UP
charToDirection 'D' = DOWN
charToDirection 'L' = LEFT
charToDirection 'R' = RIGHT

distance :: (Double, Double) -> (Double, Double) -> Double
distance (x1, y1) (x2, y2) = sqrt $ ((x2 - x1) ** 2) + ((y2 - y1) ** 2)

ropeBridge :: [Direction] -> (Double, Double) -> (Double, Double) -> [(Double, Double)]
ropeBridge [] _ _ = []
ropeBridge (x:xs) h t = t : ropeBridge xs h' t'
	where h' = move h x
	      t' = moveTail t h h'

moveTail :: (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double)
moveTail t h h' 
	| t `distance` h <= 1 = t
	| t `distance` h <= 2 = undefined


move :: (Double, Double) -> Direction -> (Double, Double)
move (x, y) UP    = (x, y + 1)
move (x, y) DOWN  = (x, y - 1)
move (x, y) LEFT  = (x - 1, y)
move (x, y) RIGHT = (x + 1, y)
