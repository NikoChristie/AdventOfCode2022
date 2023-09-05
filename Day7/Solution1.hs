import System.IO
import Data.Char
import Data.List

data File = Folder String | Document Int deriving (Show)

contents = 
	[ "$ cd /"
	, "$ ls"
	, "dir a"
	, "14848514 b.txt"
	, "8504156 c.dat"
	, "dir d"
	, "$ cd a"
	, "$ ls"
	, "dir e"
	, "29116 f"
	, "2557 g"
	, "62596 h.lst"
	, "$ cd e"
	, "$ ls"
	, "584 i"
	, "$ cd .."
	, "$ cd .."
	, "$ cd d"
	, "$ ls"
	, "4060174 j"
	, "8033020 d.log"
	, "5626152 d.ext"
	, "7214296 k" ]


main = do
	--handle <- openFile "Input.txt" ReadMode
	--contents <- hGetContents handle

	--let values = lines contents
	let table = parseFileSystem contents []

	print $ contents
	print $ table

        --hClose handle   	

parseFileSystem :: [String] -> [(String, File)] -> [(String, File)]
parseFileSystem [] table = table
parseFileSystem (x:xs) table
	| "$ cd .." == x      = parseFileSystem xs table
	| "$ cd" `isPrefixOf` x = parseFileSystem xs $ addNewFolder x table 
	| otherwise = parseFileSystem xs table

addNewFolder :: String -> [(String, File)] -> [(String, File)]
addNewFolder command table = (folderName, Folder []) : table
	where folderName = drop 5 command
