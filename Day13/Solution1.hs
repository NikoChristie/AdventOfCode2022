import System.IO  
import Control.Monad

main = do  
        handle <- openFile "Input2.txt" ReadMode
        contents <- hGetContents handle
        let signals = zipPairs $ (filter (not . null) . lines) contents

        print $ signals

        hClose handle   

zipPairs :: [a] -> [(a, a)]
zipPairs [] = []
zipPairs (x:y:xs) = [(x, y)] ++ zipPairs xs
