import System.IO
import Data.List

-- Solution #2
main = do
	let list = []
	handle <- openFile "Input.txt" ReadMode
	contents <- hGetContents handle

	let values    = map (split (==',')) $ lines contents
	    paths     = map (\(a, b) -> (tupleToRange a, tupleToRange b) ) values
	    contained = map (\(a, b) -> a `intersects` b || b `isInfixOf` a) paths
	    solution  = sum $ map (\x -> if x then 1 else 0) contained

	print $ solution

        hClose handle   	

intersects :: (Eq a) => [a] -> [a] -> Bool
intersects a b = (not . null) $ intersect a b

tupleToRange :: String -> [Int]
tupleToRange xs = [min..max]
	where (min, max) = (\(a, b) -> (read a, read b)) $ split (=='-') xs

split :: (a -> Bool) -> [a] -> ([a], [a])
split f s = (left,right)
        where
        (left,right')=break f s
        right = if null right' then [] else tail right'

