import System.IO
import Data.List
import Data.Char

--forest = [[3,0,3,7,3],[2,5,5,1,2],[6,5,3,3,2],[3,3,5,4,9],[3,5,3,9,0]]

main = do

	handle <- openFile "Input.txt" ReadMode
	contents <- hGetContents handle

	let forest = map (map (\x -> digitToInt x)) $ lines contents

	let height     = length $ transpose forest
	    width      = length forest
	    coords     = [ (x,y) | x <- [1..width-2], y <- [1..height-2] ]
	    treehouses = map (\coord -> beautyScore coord forest) coords
	    solution   = maximum treehouses
	
	print $ solution

        hClose handle   	

beautyScore :: (Int, Int) -> [[Int]] -> Int
beautyScore (x, y) grid = product $ map (score) [top, bottom, left, right]
	where value  = (grid !! y) !! x

	      score = (length . takeUntil (>= value))

              rows   = grid !! y
              cols   = (transpose grid) !! x

	      top    = reverse $ take y cols
	      bottom = drop (y + 1) cols
	      left   = reverse $ take x rows
              right  = drop (x + 1) rows

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p = foldr (\x ys -> x : if p x then [] else ys) []
