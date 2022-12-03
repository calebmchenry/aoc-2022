{-# LANGUAGE OverloadedStrings #-}
import System.IO

data Round = Round {you:: HandShape, me :: HandShape}
  deriving (Show)
data Round' = Round' {you':: HandShape, result' :: Result}
  deriving (Show)
data HandShape = Rock | Paper | Scissors
  deriving (Show)
data Result = Lose | Draw | Win
  deriving (Show)

parseHandShape :: String -> HandShape
parseHandShape "A" = Rock
parseHandShape "B" = Paper
parseHandShape "C" = Scissors
parseHandShape "X" = Rock
parseHandShape "Y" = Paper
parseHandShape "Z" = Scissors
parseHandShape x = error ("failed to parse hand shape: " ++ x)
parseResult :: String -> Result
parseResult "X" = Lose
parseResult "Y" = Draw
parseResult "Z" = Win
parseResult x = error ("failed to parse hand shape: " ++ x)

parseRound :: [String] -> Round
parseRound [x,y] = Round { you=parseHandShape x, me= parseHandShape y}
parseRound xs = error ("failed to pares Round " ++ unwords xs)

parseRound' :: [String] -> Round'
parseRound' [x,y] = Round' { you'=parseHandShape x, result'= parseResult y}
parseRound' xs = error ("failed to pares Round " ++ unwords xs)

rounds :: String -> [Round]
rounds = map (parseRound . words) .lines

rounds' :: String -> [Round']
rounds' = map (parseRound' . words) .lines

evaluate :: Round -> Int
evaluate r = outcomePoints r + choicePoints (me  r)

outcomePoints :: Round -> Int
outcomePoints Round {you=Rock, me=Scissors}= 0
outcomePoints Round {you=Paper, me=Rock}= 0
outcomePoints Round {you=Scissors, me=Paper}= 0
outcomePoints Round {you=Rock, me=Rock}= 3
outcomePoints Round {you=Paper, me=Paper}= 3
outcomePoints Round {you=Scissors, me=Scissors}= 3
outcomePoints Round {you=Rock, me=Paper}= 6
outcomePoints Round {you=Paper, me=Scissors}= 6
outcomePoints Round {you=Scissors, me=Rock}= 6

choicePoints :: HandShape -> Int
choicePoints Rock = 1
choicePoints Paper = 2
choicePoints Scissors = 3

part1 :: String -> Int
part1 = sum . map evaluate . rounds

evaluate' :: Round' -> Int
evaluate' r = resultPoints (result' r) + outcomePoints' r

outcomePoints' :: Round' -> Int
outcomePoints' Round' {you'=Rock, result'=Lose} = choicePoints Scissors
outcomePoints' Round' {you'=Rock, result'=Draw} = choicePoints Rock
outcomePoints' Round' {you'=Rock, result'=Win} = choicePoints Paper
outcomePoints' Round' {you'=Paper, result'=Lose} = choicePoints Rock
outcomePoints' Round' {you'=Paper, result'=Draw} = choicePoints Paper
outcomePoints' Round' {you'=Paper, result'=Win} = choicePoints Scissors
outcomePoints' Round' {you'=Scissors, result'=Lose} = choicePoints Paper
outcomePoints' Round' {you'=Scissors, result'=Draw} = choicePoints Scissors
outcomePoints' Round' {you'=Scissors, result'=Win} = choicePoints Rock

resultPoints :: Result -> Int
resultPoints Lose = 0
resultPoints Draw = 3
resultPoints Win = 6

part2 :: String -> Int
part2 = sum . map evaluate' . rounds'

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print $ part1 contents
  print $ part2 contents
  hClose handle
