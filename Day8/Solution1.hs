import System.IO
import Data.List
import Data.Char

--forest = [[3,0,3,7,3],[2,5,5,1,2],[6,5,3,3,2],[3,3,5,4,9],[3,5,3,9,0]]

main = do

	handle <- openFile "Input.txt" ReadMode
	contents <- hGetContents handle

	let forest = map (map (\x -> digitToInt x)) $ lines contents
	    height     = length $ transpose forest
	    width      = length forest
	    coords     = [ (x,y) | x <- [1..width-2], y <- [1..height-2] ]
	    treehouses = map (\coord -> visible coord forest) coords
	    solution   = (height * 2) + ((width - 2) * 2) + (sum $ map (\x -> if x then 1 else 0) treehouses)
	
	print $ solution

        hClose handle   	

visible :: (Int, Int) -> [[Int]] -> Bool
visible (x, y) grid = (all (< value) left || all (< value) right) || (all (< value) top || all (< value) bottom)
	where value  = (grid !! y) !! x

              rows   = grid !! y
              cols   = (transpose grid) !! x

	      top    = take y cols
	      bottom = drop (y + 1) cols
	      left   = take x rows
              right  = drop (x + 1) rows

