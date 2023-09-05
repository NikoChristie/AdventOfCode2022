import System.IO
import Data.List

-- Too High 13084 

-- Solution #1
main = do
	let list = []
	handle <- openFile "Input.txt" ReadMode
	contents <- hGetContents handle

	let moves'   = (map (\xs -> splitAtElement ',' xs) . lines) contents
	    moves    = map (\(a:b:c:_) -> (read a, read b, read c)) moves' :: [(Int, Int, Int)]
	    mapSize  = size moves
	    solution = surfaceArea moves moves


	print $ solution
	print $ mapSize

        hClose handle   	

splitAtElement :: Eq a => a -> [a] -> [[a]]
splitAtElement x l =
  case l of
    []          -> []
    e:es | e==x -> split es
    es          -> split es
  where
    split es = let (first,rest) = break (x==) es
               in first : splitAtElement x rest

surfaceArea :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> Int
surfaceArea [] _ = 0
surfaceArea ((x, y, z):cs) cubes = ((length . filter (==False) . map (\x -> x `elem` cubes)) [up, down, front, back, left, right]) + surfaceArea cs cubes
	where up     = (x, y, z + 1)
	      down   = (x, y, z - 1)
              front  = (x, y + 1, z)
	      back   = (x, y - 1, z)  
	      left   = (x + 1, y, z)
	      right  = (x - 1, y, z)

size :: [(Int, Int, Int)] -> (Int, Int, Int)
size xs = (x, y, z)
	where x = maximum $ map (\(x,_,_) -> x) xs
	      y = maximum $ map (\(_,y,_) -> y) xs 
	      z = maximum $ map (\(_,_,z) -> z) xs

-- iter thru all possible points and find trapped, subtract (*6) from solution
