import System.IO
import Data.List
import Text.Read
import Data.Maybe

answer = [ [1, 2, -3, 3, -2, 0, 4]
	 , [2, 1, -3, 3, -2, 0, 4]
	 , [1, -3, 2, 3, -2, 0, 4]
	 , [1, 2, 3, -2, -3, 0, 4]
	 , [1, 2, -2, -3, 0, 3, 4]
	 , [1, 2, -3, 0, 3, 4, -2]
	 , [1, 2, -3, 0, 3, 4, -2] 
	 , [1, 2, -3, 4, 0, 3, -2] ]
{-
Initial arrangement:
1, 2, -3, 3, -2, 0, 4

1 moves between 2 and -3:
2, 1, -3, 3, -2, 0, 4

2 moves between -3 and 3:
1, -3, 2, 3, -2, 0, 4

-3 moves between -2 and 0:
1, 2, 3, -2, -3, 0, 4

3 moves between 0 and 4:
1, 2, -2, -3, 0, 3, 4

-2 moves between 4 and 1:
1, 2, -3, 0, 3, 4, -2

0 does not move:
1, 2, -3, 0, 3, 4, -2

4 moves between -3 and 0:
1, 2, -3, 4, 0, 3, -2
 - -}

-- Solution #1
main = do
	handle <- openFile "Input2.txt" ReadMode
	contents <- hGetContents handle

	let list = map (read) $ lines contents :: [Int]

	print $ encrypt [] list
	print $ encrypt [1] list
	print $ encrypt [1, 2] list
	print $ encrypt [1, 2, -3] list
	print $ encrypt [1, 2, -3, 3] list
	print $ encrypt [1, 2, -3, 3] list 
	print $ encrypt [1, 2, -3, 3, -2] list
	print $ encrypt [1, 2, -3, 3, -2, 0] list -- !!!
	print $ encrypt [1, 2, -3, 3, -2, 0, 4] list
	print $ groveCoords (encrypt [1, 2, -3, 3, -2, 0, 4] list)

	putStr $ "\nANSWERS\n\n"

	putStr $ unlines $ map (\x -> (show x)) answer
	--print $ encrypt list list

        hClose handle   	

encrypt :: [Int] -> [Int] -> [Int]
encrypt [] ys = ys
encrypt (0:xs) ys = encrypt xs ys
encrypt (x:xs) ys = encrypt xs (left ++ [x] ++ right)
	where index = (fromJust . elemIndex x) ys
	      i     = if x < 0 then (index + (x - 1)) `mod` length ys else (index + x) `mod` length ys
	      ys'   = remove x ys
	      left  = take (i + 1) ys'
	      right = drop (i + 1) ys'

-- (index + offset) `mod` length

remove :: (Eq a) => a -> [a] -> [a]
remove _ [] = []
remove y (x:xs) 
	| x == y = xs
	| otherwise = x : remove y xs

groveCoords :: [Int] -> Int
groveCoords xs = a + b + c
	where zero = fromJust $ elemIndex 0 xs
	      len  = length xs
 	      a    = xs !! (zero + (1000 `mod` len)) `mod` len
 	      b    = xs !! (zero + (2000 `mod` len)) `mod` len
 	      c    = xs !! (zero + (3000 `mod` len)) `mod` len

