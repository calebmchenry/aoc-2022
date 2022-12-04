{-# LANGUAGE OverloadedStrings #-}
import System.IO  
import qualified Data.Text as T

sections :: String -> [Int]
sections = map (read . T.unpack) . T.splitOn "-" . T.pack

pairs :: String -> [String]
pairs = map T.unpack . T.splitOn "," . T.pack

parseLine :: String -> [[Int]]
parseLine = map sections . pairs

fullyContains :: [[Int]] -> Bool
fullyContains [[a, b], [x, y]]= (a <= x && b >= y) || (x <= a && y >= b)
fullyContains x = error "fullyContains did not have two arrays of two values"

overlap :: [[Int]] -> Bool
overlap [[a, b], [x, y]]= (a <= x && b >= x) || (a <= y && b >= y) || (x <= a && y >= a) || (x <= b && y >= b)
overlap x = error "overlap did not have two arrays of two values"

part1 :: String -> Int
part1 = length . filter fullyContains . map parseLine . lines 

part2 :: String -> Int
part2 = length . filter overlap . map parseLine . lines 


main :: IO ()
main = do  
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print $ part1 contents
  print $ part2 contents
  hClose handle
