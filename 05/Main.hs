{-# LANGUAGE OverloadedStrings #-}
import System.IO
import qualified Data.Text as T

-- Parsing is hard but hard coding is on par 📈
-- Treat the stacks backwards so that I can do front list operations which are 
-- easier (e.g. take / drop)
initialStacks :: [[Char]]
initialStacks = [
  reverse "GDVZJSB",
  reverse "ZSMGVP",
  reverse "CLBSWTQF",
  reverse "HJGWMRVQ",
  reverse "CLSNFMD",
  reverse "RGCD",
  reverse "HGTRJDSQ",
  reverse "PFV",
  reverse "DRSTJ"]

data Move = Move Int Int Int

parseMove :: String -> Move
parseMove s = Move n f t
  where
    [_, n, _, f, _, t] = map read $ words s

parseMoves :: String ->  [Move]
parseMoves c = map parseMove $ lines str
  where
    str = T.unpack $ T.splitOn "\n\n" (T.pack c) !! 1

replace :: [String] -> (Int, String) -> [String]
replace [] _ = []
replace (_:xs) (0,a) = a:xs
replace (x:xs) (n,a) =
  if n < 0
    then x:xs
    else x: replace xs (n-1,a)

move :: Int -> String -> String -> (String, String)
move n f t = (newF, newT)
  where
    newF = drop n f
    newT = reverse (take n f) ++ t


moveCrates :: [[Char]] -> Move -> [[Char]]
moveCrates ss (Move n f t) = replace (replace ss (fi, nf)) (ti, nt)
  where
    fi =  f - 1
    ti =  t - 1
    (nf, nt) = move n (ss !! fi) (ss !! ti )

part1 :: String -> String
part1 c = map head $ foldl moveCrates initialStacks $ parseMoves c

move' :: Int -> String -> String -> (String, String)
move' n f t = (newF, newT)
  where
    newF = drop n f
    newT = take n f ++ t

moveCrates' :: [[Char]] -> Move -> [[Char]]
moveCrates' ss (Move n f t)= replace (replace ss (fi, nf)) (ti, nt)
  where
    fi =  f - 1
    ti =  t - 1
    (nf, nt) = move' n (ss !! fi) (ss !! ti )

part2 :: String -> String
part2 c = map head $ foldl moveCrates' initialStacks $ parseMoves c

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print $ part1 contents
  print $ part2 contents
  hClose handle
