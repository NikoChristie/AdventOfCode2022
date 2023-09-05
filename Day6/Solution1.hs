import System.IO
import Data.List

main = do
	handle <- openFile "Input.txt" ReadMode
	contents <- hGetContents handle

	let packetStart       = length contents
	    packetStartMarker = length $ degarbble contents
	    solution          = (packetStart - packetStartMarker) + 1
            


	print $ solution

        hClose handle   	

degarbble :: String -> String
degarbble [] = []
degarbble (a:b:c:d:xs) = if all (==1) $ map (\x -> count x four) four
			 then a : b : c : d : xs
			 else degarbble xs
	where four = [a, b, c, d]

count :: Eq a => Integral b => a -> [a] -> b
count e [] = 0
count e (a:xs) = (count e xs +) $ if a == e then 1 else 0
