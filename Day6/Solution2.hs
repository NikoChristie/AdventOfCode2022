import System.IO
import Data.List

main = do
	handle <- openFile "Input.txt" ReadMode
	contents <- hGetContents handle

	let packetStart       = length contents
	    packetStartMarker = length $ degarbble contents
	    solution          = (packetStart - packetStartMarker)
            


	print $ solution

        hClose handle   	

degarbble :: String -> String
degarbble [] = []
degarbble xs = if all (==1) $ map (\x -> count x fourteen) fourteen
			 then drop 14 xs
			 else degarbble (tail xs)
	where fourteen = take 14 xs

count :: Eq a => Integral b => a -> [a] -> b
count e [] = 0
count e (a:xs) = (count e xs +) $ if a == e then 1 else 0
