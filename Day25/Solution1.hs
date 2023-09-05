import System.IO
import Data.List

-- Solution #1
main = do
	let list = []
	handle <- openFile "Input.txt" ReadMode
	contents <- hGetContents handle

	let answer = (sum . (map (snafuToDecimal)) . lines) contents

	print $ decimalToSnafu answer


        hClose handle   	

snafuToDecimal :: String -> Int
snafuToDecimal [] = 0
snafuToDecimal (x:xs) = (snafuRead x) * (5 ^ (length xs)) + snafuToDecimal xs

snafuRead :: Char -> Int
snafuRead '=' = -2
snafuRead '-' = -1
snafuRead '0' =  0
snafuRead '1' =  1
snafuRead '2' =  2

decimalToSnafu :: Int -> String
decimalToSnafu 0 = ""
decimalToSnafu x = (decimalToSnafu next) ++ [x']
	where i = (fromIntegral x) / 5.0
	      next = round i
	      x' = decimalRead (x - (next) * 5)

decimalRead :: Int -> Char
decimalRead (-2) = '='
decimalRead (-1) = '-'
decimalRead  0 = '0'
decimalRead  1 = '1'
decimalRead  2 = '2'

round' :: (RealFrac a) => a -> Int
round' x = (round x) :: Int
