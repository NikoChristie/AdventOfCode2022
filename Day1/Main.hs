import System.IO
import Data.List

{-
-- Solution #1
main = do
	let list = []
	handle <- openFile "Input.txt" ReadMode
	contents <- hGetContents handle

	let elvesString = splitList "" $ lines contents
            elves = map (\x -> (map read x)) elvesString :: [[Integer]]
	    sorted = sortBy compare $ map sum elves
	
	print $ last sorted

        hClose handle   	
-}
-- Solution #2
main = do
	handle <- openFile "Input.txt" ReadMode
	contents <- hGetContents handle

	let elvesString = splitList "" $ lines contents
            elves = map (\x -> (map read x)) elvesString :: [[Integer]]
	    sorted = sortBy compare $ map sum elves
	

	print $ sum (take 3 (reverse sorted))

        hClose handle   	



splitList :: Eq a => a -> [a] -> [[a]]
splitList _   [] = []
splitList sep list = h:splitList sep t
        where (h,t)=split (==sep) list
split :: (a -> Bool) -> [a] -> ([a], [a])
split f s = (left,right)
        where
        (left,right')=break f s
        right = if null right' then [] else tail right'
