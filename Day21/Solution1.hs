import System.IO
import Data.List
import Text.Read
import Data.Maybe


-- Solution #1
main = do
	let list = []
	handle <- openFile "Input.txt" ReadMode
	contents <- hGetContents handle

	let monkeys = (map (\(a, b) -> (init a, words b)) . map (splitAt 5)) $ lines contents

	print $ fetchNumber monkeys "root"

        hClose handle   	

readSymbol :: String -> (Int -> Int -> Int)
readSymbol "+" = (+)
readSymbol "-" = (-)
readSymbol "/" = div
readSymbol "*" = (*)
readSymbol  x  = error $ x ++ " is not a recognized symbol"

fetchNumber :: [(String, [String])] -> String -> Int
fetchNumber ds name 
	| length next == 1 = (read . head) next
	| otherwise        = (\(left:symbol:right:_) -> ((readSymbol symbol) (fetchNumber ds left) (fetchNumber ds right))) next
	where next = fromJust $ lookup name ds 

splitAtElement :: Eq a => a -> [a] -> [[a]]
splitAtElement x l =
  case l of
    []          -> []
    e:es | e==x -> split es
    es          -> split es
  where
    split es = let (first,rest) = break (x==) es
               in first : splitAtElement x rest


