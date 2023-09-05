import System.IO
import Data.List
import Data.Char

{-
 
[V]     [B]                     [C]
[C]     [N] [G]         [W]     [P]
[W]     [C] [Q] [S]     [C]     [M]
[L]     [W] [B] [Z]     [F] [S] [V]
[R]     [G] [H] [F] [P] [V] [M] [T]
[M] [L] [R] [D] [L] [N] [P] [D] [W]
[F] [Q] [S] [C] [G] [G] [Z] [P] [N]
[Q] [D] [P] [L] [V] [D] [D] [C] [Z]
 1   2   3   4   5   6   7   8   9  
-}

crates = ["VCWLRMFQ", "LQD", "BNCWGRSP", "GQBHDCL", "SZFLGV", "PNGD", "WCFVPZD", "SMDPC", "CPMVTWNZ"] :: [String]

main = do
	handle <- openFile "Input.txt" ReadMode
	contents <- hGetContents handle

	let moves'  = map (\(_:amount:_:start:_:end:_) -> (read amount, (read start, read end))) $ map (words) ((drop 10) (lines contents)) :: [((Int), (Int, Int))]
            moves   = map (\(amount, (start, end)) -> (amount, (start - 1, end - 1))) moves'
            solution = map (head) $ crane moves crates  

	print $ moves
	print $ solution

        hClose handle   	

crane :: [(Int, (Int, Int))] -> [[Char]] -> [[Char]]
crane []     crate = crate
crane (x:xs) crate = crane xs (craneMove x crate)

craneMove :: (Int, (Int, Int)) -> [[Char]] -> [[Char]]
craneMove (n, (from, to)) crate = crate'
	where box    = take n (crate !! from)
	      crate' = cranePlaceUpon box to $ craneTakeOff from n crate

craneTakeOff :: Int -> Int -> [[Char]] -> [[Char]]
craneTakeOff 0 n (x:xs) = (drop n x) : xs
craneTakeOff i n (x:xs) = x : craneTakeOff (i - 1) n xs

cranePlaceUpon :: [Char] -> Int -> [[Char]] -> [[Char]]
cranePlaceUpon box 0 (x:xs) = (box ++ x) : xs
cranePlaceUpon box i (x:xs) = x : cranePlaceUpon box (i - 1) xs
