import System.IO
import Data.Char
import Data.List

main = do
	handle <- openFile "Input.txt" ReadMode
	contents <- hGetContents handle

	let values        = lines contents
	    rucksacks     = map (\(a, b, c) -> head (intersectBy (==) (intersectBy (==) a b) c)) $ bunch values
            solution      = sum $ map score rucksacks

            --rucksacks     = map (\x -> splitAt ((length x) `div` 2) x) values
            --rucksacks     = map (splitInThree) values
	    --intersections = map (head) $ map (\(a, b, c) -> intersectBy (==) (intersectBy (==) a b) c) rucksacks :: [Char]
	    --intersections = map (head) $ filter (not . null) (map (\(a, b, c) -> intersectBy (==) (intersectBy (==) a b) c) rucksacks)
	    --solution      = sum $ map score intersections

	print $ rucksacks
	--print $ intersections
	print $ solution

        hClose handle   	

splitInThree :: [a] -> ([a], [a], [a])
splitInThree xs = (a, b, c)
	where n = length xs `div` 3
	      (a,as) = splitAt n xs
	      (b,c)  = splitAt n as


bunch :: [a] -> [(a, a, a)]
bunch [] = []
bunch (x:y:z:xs) = [(x, y, z)] ++ bunch xs
	       

score :: Char -> Int
score 'a' = 1
score 'b' = 2
score 'c' = 3
score 'd' = 4
score 'e' = 5
score 'f' = 6
score 'g' = 7
score 'h' = 8
score 'i' = 9
score 'j' = 10
score 'k' = 11
score 'l' = 12
score 'm' = 13
score 'n' = 14
score 'o' = 15
score 'p' = 16
score 'q' = 17
score 'r' = 18
score 's' = 19
score 't' = 20
score 'u' = 21
score 'v' = 22
score 'w' = 23
score 'x' = 24
score 'y' = 25
score 'z' = 26
score 'A' = 27
score 'B' = 28
score 'C' = 29
score 'D' = 30
score 'E' = 31
score 'F' = 32
score 'G' = 33
score 'H' = 34
score 'I' = 35
score 'J' = 36
score 'K' = 37
score 'L' = 38
score 'M' = 39
score 'N' = 40
score 'O' = 41
score 'P' = 42
score 'Q' = 43
score 'R' = 44
score 'S' = 45
score 'T' = 46
score 'U' = 47
score 'V' = 48
score 'W' = 49
score 'X' = 50
score 'Y' = 51
score 'Z' = 52
