{-# LANGUAGE OverloadedStrings #-}
import System.IO
import Data.Char (ord, isLower)
import Data.List (intersect)

lowerCasePriority :: Char -> Int
lowerCasePriority c = ord c - 96

upperCasePriority :: Char -> Int
upperCasePriority c = ord c - 38

priority :: Char -> Int
priority c = if isLower c then lowerCasePriority c else upperCasePriority c

commonChar2 :: String -> String -> Char
commonChar2 xs ys =  head $ intersect xs ys

splitHalf :: [a] -> ([a], [a])
splitHalf l = splitAt ((length l + 1) `div` 2) l

part1 :: String -> Int
part1 = sum . map (priority . uncurry commonChar2 . splitHalf ) . lines

intersect3 :: String -> String -> String -> String
intersect3 a b c  =  intersect a $ intersect b c

commonChar3 :: [String] -> Char
commonChar3 [a,b, c] = head $ intersect3 a b c
commonChar3 x = error "did not provide array of three to getMatch3"

group3 :: [String] -> [[String]]
group3 [] = []
group3 xs = take 3 xs : group3 (drop 3 xs)
 
part2 :: String -> Int
part2 = sum . map (priority . commonChar3 ) . group3 . lines


main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print $ part1 contents
  print $ part2 contents
  hClose handle
