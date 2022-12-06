{-# LANGUAGE OverloadedStrings #-}
import System.IO
import qualified Data.Text as T
import Data.List (tails, nub)

windows :: Int -> [a] -> [[a]]
windows m = foldr (zipWith (:)) (repeat []) . take m . tails

part1 :: String -> Int 
part1 =  (+ 4) . length . takeWhile ((/= 4) . length ) . map nub . windows 4

part2 :: String -> Int 
part2 =  (+ 14) . length . takeWhile ((/= 14) . length ) . map nub . windows 14

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print $ part1 contents
  print $ part2 contents
  hClose handle
