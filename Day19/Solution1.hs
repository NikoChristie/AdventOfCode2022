import System.IO
import Data.List
import Data.Maybe

data Resource = Ore | Clay | Obsidian | Geode
	deriving (Show, Eq)

type Robot = (Resource, [(Resource, Int)])

resourcesEmpty = [ (Ore, 0)
		 , (Clay, 0)
		 , (Obsidian, 0)
		 , (Geode, 0) ]

input = "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 17 clay. Each geode robot costs 4 ore and 20 obsidian."

-- Solution #1
main = do
	let list = []
	handle <- openFile "Input.txt" ReadMode
	contents <- hGetContents handle

	let bluePrints = ((map (stringToRobot)) . filter (not . null) . (splitAtElement '.') . dropWhile (/='E')) input 
 	    solution   = mine [] bluePrints resourcesEmpty 24 

	print $ bluePrints

        hClose handle   	

mine :: [Resource] -> [Robot] -> [(Resource, Int)] -> Int -> Int
mine _ _ resources 0 = fromJust $ lookup Geode resources
mine robots bluePrints resources i = mine robots' bluePrints resources' (i - 1)
	where newRobots  = undefined
	      robots'    = newRobots ++ robots
	      resources' = map(\(r, n) -> (r, n + (length . filter (==r)) robots)) resources -- collect resources from bots

buyNewRobots :: [Robot] -> [Resource] -> [(Resource, Int)] -> ([Resource], [(Resource, Int)])
buyNewRobots [] robots resources = (robots, resources)
buyNewRobots (b:bs) robots resources = buyNewRobots bs robots' resources'
	where newRobots = replicate (afford b resources) (fst b)
	      robots'   = robots ++ newRobots
	      resources' = spend newRobots resources

--afford :: Robot -> [(Resource, Int)] -> Bool
--afford (_, cost) resources = all (\(r, n) -> (fromJust . lookup (r)) resources >= n) resources

afford :: Robot -> [(Resource, Int)] -> Int
afford (_, cost) resources = minimum (\(r, n) -> (fromJust . lookup (r)) resources `div` n) resources

spend :: [Resource] -> [(Resource, Int)] -> [(Resource, Int)]
spend bluePrint resources = map (\(r, n) -> if r `elem` (map (fst) cost) then (r, n - afford bluePrint) else (r, n)) resources
	where cost = (\(_, x) -> x) bluePrint


stringToRobot :: String -> Robot
stringToRobot s = (robotType, cost)
	where robotType = (readResource . (!! 1) . words) s
	      cost      = ((map (\(n:r:_) -> (readResource r, read n :: Int) )) . (splitAtElement ("and")) . (drop 4) . words) s

readResource :: String -> Resource
readResource "ore" = Ore
readResource "clay" = Clay
readResource "obsidian" = Obsidian
readResource "geode" = Geode
readResource x = error $ "Unexpected Value \"" ++ x ++ "\""

splitAtElement :: Eq a => a -> [a] -> [[a]]
splitAtElement x l =
  case l of
    []          -> []
    e:es | e==x -> split es
    es          -> split es
  where
    split es = let (first,rest) = break (x==) es
               in first : splitAtElement x rest


