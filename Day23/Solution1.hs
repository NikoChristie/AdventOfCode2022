import System.IO
import Data.List
--import Data.List.Extra
import Data.Char

main = do
	handle <- openFile "Input2.txt" ReadMode
	contents <- hGetContents handle

	let grid = lines contents
	    width  = (length . head) grid
	    height = length grid 
	    elves = filter (\(x, y) -> ((grid !! y) !! x) == '#') $ positions width height

	print $ elves
	print $ printGrid elves width height

        hClose handle   	

printGrid :: [(Int, Int)] -> Int -> Int -> [String]
printGrid elves width height = chunksOf width $ map (\p -> if p `elem` elves then '#' else '.') pos
	where pos = positions width height

positions :: Int -> Int -> [(Int, Int)]
positions width height = foldl (++) [] $ map (\y -> map (\x -> (x, y)) [0..(width - 1)]) [0..(height - 1)]

proposedPosition :: (Int, Int) -> [(Int, Int)] -> (Int, Int)
proposedPosition (x, y) elves 
	| not (n `elem` elves && nw `elem` elves && ne `elem` elves) = n
	| not (s `elem` elves && sw `elem` elves && se `elem` elves) = s
	| not (e `elem` elves && ne `elem` elves && se `elem` elves) = e
	| not (w `elem` elves && nw `elem` elves && sw `elem` elves) = w
	| otherwise = (x, y)
	where n = (x, y - 1)
	      s = (x, y + 1)
	      e = (x + 1, y)
	      w = (x - 1, y)
	      ne = (x + 1, y - 1)
	      nw = (x - 1, y - 1)
	      se = (x + 1, y + 1)
	      sw = (x - 1, y + 1)
	 

count :: (Eq a) => [a] -> a -> Int
count [] _ = 0
count (x:xs) x'
	| x == x'   = 1 + count xs x'
	| otherwise = 0 + count xs x'

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf i xs = (take i xs) : chunksOf i (drop i xs)
