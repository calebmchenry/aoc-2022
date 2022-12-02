{-# LANGUAGE OverloadedStrings #-}
import System.IO  
import qualified Data.Text as T
import Data.List

inventories :: String -> [String]
inventories = map T.unpack . T.splitOn "\n\n" . T.pack

snacks ::  String -> [Int]
snacks = map read . lines

elfCalories :: String -> [Int]
elfCalories = map (sum . snacks) . inventories

part1 :: String -> Int
part1 = maximum . elfCalories

topthree :: [Int] -> Int
topthree = sum . take 3 . reverse . sort

part2 :: String -> Int
part2 = topthree . elfCalories

main :: IO ()
main = do  
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print $ part1 contents
  print $ part2 contents
  hClose handle
