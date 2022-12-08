{-# LANGUAGE OverloadedStrings #-}
import System.IO
import Data.List

data OutputLine = ChangeDir String | Dir String | File Int String | List
  deriving (Show)

data State = State String [(String, Int)]
  deriving (Show)

parseLine :: [String] -> OutputLine
parseLine ("$":"cd":dir:xs) = ChangeDir dir
parseLine ("$":"ls":xs) = List
parseLine ("dir":dir:xs) = Dir dir
parseLine (size:name:xs) = File (read size) name
parseLine xs = error ("Unable to parse line " ++ unwords xs)

upsert :: String -> Int -> [(String, Int)] -> [(String, Int)]
upsert key value xs = if exists then (key, (+ value) $ snd $ head fs):ufs else (key, value):xs
  where
    fs = filter ((== key) . fst) xs
    ufs = filter ((/= key) . fst) xs
    exists = (== 1) $ length fs

upDir :: String -> String
upDir = reverse . tail . dropWhile (/= '.') . reverse

eval :: State -> OutputLine -> State
eval (State _ dirs) (ChangeDir "/") = State "/" dirs
eval (State cwd dirs) (ChangeDir "..") = State (upDir cwd) dirs
eval (State cwd dirs) (ChangeDir dir) = State newCwd (upsert cwd 0 dirs)
  where
    newCwd = cwd ++ "." ++ dir
eval state (Dir dir) = state
eval (State cwd dirs) (File size name) = State cwd (upsert cwd size dirs)
eval s List = s

sumDir :: [(String, Int)] -> (String, Int) ->  Int
sumDir ds (dir, size) =  size + sumOfChildren
  where
    sumOfChildren = sum $ map snd $ filter ( (/= dir). fst) $ filter (isPrefixOf dir . fst) ds

sumDirs :: [(String, Int)] -> [Int]
sumDirs ds = map (sumDir ds)  ds

stateDirs :: State -> [(String, Int)]
stateDirs (State _ dirs) = dirs

parseInput :: String -> [OutputLine]
parseInput = map (parseLine . words) . lines

buildDirSizes :: [OutputLine] -> [Int]
buildDirSizes = sumDirs . stateDirs. foldl eval (State "" []) 

part1 :: String -> Int
part1 = sum . filter (<= 100000) . buildDirSizes . parseInput

fileSystemSize :: Int
fileSystemSize = 70000000
freeSpaceNeeded :: Int
freeSpaceNeeded = 30000000

part2 :: String -> Int
part2 c = minimum $ filter ( >= remainingFreeSpaceNeeded) sizes
  where
    sizes = buildDirSizes $ parseInput c
    occupiedSpace = maximum sizes
    remainingFreeSpaceNeeded = freeSpaceNeeded - (fileSystemSize - occupiedSpace)

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print $ part1 contents
  print $ part2 contents
  hClose handle
