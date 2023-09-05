import System.IO
import Data.List

-- Solution #1
main = do
	handle <- openFile "Input.txt" ReadMode
	contents <- hGetContents handle

	let movesRaw    = map (\x -> split (==' ') x) $ lines contents
	    moves       = map (\(a, b) -> (symbolToChoice a, symbolToOutcome b)) movesRaw
	    moves'      = map (\(a, b) -> (a, choiceAndOutcome a b)) moves
            winScore    = sum $ map (\x -> outcomeToScore (shoot x)) moves'
   	    choiceScore = sum $ map (\(_, x) -> choiceToScore x) moves'
	    score = winScore + choiceScore

	print $ score

        hClose handle   	

data Choice = Rock | Paper | Scissors deriving (Show)
data Outcome = Win | Tie | Loss

outcomeToScore :: Outcome -> Int
outcomeToScore Win  = 6
outcomeToScore Tie  = 3
outcomeToScore Loss = 0

choiceToScore :: Choice -> Int
choiceToScore Rock = 1
choiceToScore Paper = 2
choiceToScore Scissors = 3

choiceAndOutcome :: Choice -> Outcome -> Choice
choiceAndOutcome Rock Win      = Paper
choiceAndOutcome Rock Loss     = Scissors
choiceAndOutcome Paper Win     = Scissors
choiceAndOutcome Paper Loss    = Rock
choiceAndOutcome Scissors Win  = Rock
choiceAndOutcome Scissors Loss = Paper
choiceAndOutcome x Tie         = x

shoot :: (Choice, Choice) -> Outcome
shoot (Rock, Scissors)     = Loss
shoot (Scissors, Paper)    = Loss 
shoot (Paper, Rock)        = Loss
shoot (Scissors, Rock)     = Win
shoot (Paper, Scissors)    = Win
shoot (Rock, Paper)        = Win
shoot (Rock, Rock)         = Tie
shoot (Paper, Paper)       = Tie
shoot (Scissors, Scissors) = Tie

symbolToChoice :: String -> Choice
symbolToChoice "A" = Rock
symbolToChoice "B" = Paper
symbolToChoice "C" = Scissors

symbolToOutcome :: String -> Outcome
symbolToOutcome "X" = Loss
symbolToOutcome "Y" = Tie
symbolToOutcome "Z" = Win

split :: (a -> Bool) -> [a] -> ([a], [a])
split f s = (left,right)
        where
        (left,right')=break f s
        right = if null right' then [] else tail right'

