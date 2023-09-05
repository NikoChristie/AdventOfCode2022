import Data.List
{-
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
-}

main = do
	let solution = monkeyRound monkeys 20

	print $ solution

data Monkey = Monkey 
	{ items     :: [Int]
	, operation :: Int -> Int
	, throw     :: (Int, (Int, Int))
	, inspectionCounter :: Int
	} 

monkeys = [ Monkey [79, 98] (\x -> x * 19) (23, (2, 3)) 0
	  , Monkey [54, 65, 75, 74] (\x -> x + 6) (19, (2, 0)) 0
	  , Monkey [79, 60, 97] (\x -> x * x) (13, (1, 3)) 0
	  , Monkey [74] (\x -> x + 3) (17, (0, 1)) 0
	  ]

monkeyRound :: [Monkey] -> Int -> Int
monkeyRound ms 0 = (product . (take 2) . reverse . sort . map (inspectionCounter)) ms -- product of two most active monkeys
monkeyRound ms i = monkeyRound ms' (i - 1)
	where ms' = map (monkeyTurn) ms

monkeyTurn :: [Monkey] -> [Monkey]
monkeyTurn [] = []
monkeyTurn ((Monkey items operation throw inspectionCounter):ms) 

-- Monkey i looks at item
-- Monkey i gets bored
-- Monkey i throws to Monkey j

-- Monkeys ItemValue ThrowerId CatcherId Monkeys'
{-
monkeyThrow :: [Monkey] -> Int -> Int -> Int -> [Monkey]
monkeyThrow [] _ _ _ = []
monkeyThrow (m:ms) item throwerId catcherId
	| index == throwerId = thrower' : throw ms
	| index == catcherId = catcher' : throw ms
	where index    = (length monkeys) - (length ms)
	      thrower' = (\(Monkey items opertaion throw inspectionCounter) -> (Monkey (filter (==item) items) operation throw inspectionCounter)) m
	      catcher' = (\(Monkey items opertaion throw inspectionCounter) -> (Monkey (item:items) operation throw inspectionCounter)) m
-}

