{-# LANGUAGE OverloadedStrings #-}
import System.IO
import Data.List
import Data.Char (digitToInt)

trees :: String -> [[Int]]
trees = map (map digitToInt) . lines

visible :: Int -> [Int] -> [Int] -> Bool
visible v l r = all (< v) l || all (< v) r

visibleRow :: [Int] -> [Int] -> [Bool]
visibleRow ls (t:rs) = visible t ls rs:visibleRow (ls++[t]) rs
visibleRow ls [] = []

part1 :: String -> Int
part1 c = length $ filter id $ concat $ zipWith (zipWith (||)) rv cv
  where
    ts = trees c
    rv = map (visibleRow []) ts
    cv = transpose $ map (visibleRow []) $ transpose ts

scenicL :: Int -> [Int] -> Int
scenicL v = scenicR v . reverse

scenicR :: Int -> [Int] -> Int
scenicR v rs | null rs = 0
             | otherwise = if vLen == oLen then vLen else 1 + vLen
                where
                  view = takeWhile (< v) rs
                  vLen = length view
                  oLen = length rs

scenic :: Int -> [Int] -> [Int] -> Int
scenic v l r =  scenicL v l * scenicR v r

scenicRow :: [Int] -> [Int] -> [Int]
scenicRow ls (t:rs) = scenic t ls rs:scenicRow (ls++[t]) rs
scenicRow ls [] = []

part2 :: String -> Int
part2 c = maximum $ concat $ zipWith (zipWith (*)) rs cs
  where
    ts = trees c
    rs = map (scenicRow []) ts
    cs = transpose $ map (scenicRow []) $ transpose ts

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print $ part1 contents
  print $ part2 contents
  hClose handle
