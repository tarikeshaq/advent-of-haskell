module Day4 where

import Day3(isDigit)
import Debug.Trace(trace)
import Data.List(elem)

data Card = Card {
  cardId :: Int,
  winning :: [Int],
  available :: [Int]
} deriving (Show)

getPoints :: Card -> Int 
getPoints Card { winning = winning, available = available } =
     let numWinning = foldr (\x acc -> 
                      if x `elem` winning then acc + 1 else acc) 0 available
     in if numWinning == 0 then 0 else 2 ^ (numWinning - 1) 

parseListWithAcc :: [Int] -> String -> ([Int], String)
parseListWithAcc acc [] = (acc, []) 
parseListWithAcc acc (' ' : xs) = 
  parseListWithAcc acc xs 
parseListWithAcc acc (x : xs)
 | isDigit x = let (num, rem) = parseNum (x: xs)
               in parseListWithAcc (num : acc) rem
 | otherwise = (acc, x : xs)

parseList :: String -> ([Int], String)
parseList = parseListWithAcc [] 


parseNumWithAcc :: String -> String -> (Int, String)
parseNumWithAcc acc [] = (read $ reverse acc, [])
parseNumWithAcc acc (x : xs)
  | isDigit x = parseNumWithAcc (x : acc) xs
  | otherwise = (read $ reverse acc, x : xs)


parseNum :: String -> (Int, String)
parseNum = parseNumWithAcc [] 

parseId :: String -> (Int, String)
parseId (x : xs)
  | isDigit x = let (res, ':' : ' ' : rem) = parseNum (x : xs)
                in (res, rem) 
  | otherwise = parseId xs

parseAvailable :: String -> ([Int], String)
parseAvailable = parseList 


parseWinning :: String -> ([Int], String)
parseWinning s = 
   let (res, '|' : ' ' : rem) = parseList s
   in (res, rem)


parse :: String -> Card
parse s = 
    let (cardId, afterId) = parseId s
        (winning, afterWinning) = parseWinning afterId 
        (available, _) = parseAvailable afterWinning
    in 
    Card {
      cardId = cardId,
      winning = winning,
      available = available 
    }
-- Approach:
-- First, we parse the input into a list of Cards, each including its list
-- of winning numbers, and list of numbers you have 
-- Then, we map the list of cards, to be a list of points 
-- Finally, we get the sum of the points
solve1 :: String -> String
solve1 = show . sum . map ( getPoints . parse) . lines

solve2 :: String -> String
solve2 _ = "world"
