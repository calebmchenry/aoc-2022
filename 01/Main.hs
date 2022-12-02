{-# LANGUAGE OverloadedStrings #-}
import System.IO  
import qualified Data.Text as T
import Data.List

inventories :: T.Text -> [String]
inventories = map T.unpack . T.splitOn "\n\n"

snacks ::  String -> [Int]
snacks = map read . lines

calories :: [Int] -> Int
calories = sum

part1 :: String -> Int
part1 = maximum . map (calories . snacks) . inventories . T.pack

part2 :: String -> Int
part2 = sum . take 3 . reverse . sort . map (calories . snacks) . inventories . T.pack

main :: IO ()
main = do  
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print $ part1 contents
  print $ part2 contents
  hClose handle
