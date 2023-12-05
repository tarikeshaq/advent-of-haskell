module Day4 where

import Day3(isDigit)
import Debug.Trace(trace)
import Data.List(elem)

data Card = Card {
  cardId :: Int,
  winning :: [Int],
  available :: [Int]
} deriving (Show)



numMatches :: Card -> Int 
numMatches Card { winning = winning, available = available } = foldr (\x acc -> 
                      if x `elem` winning then acc + 1 else acc) 0 available
 


points :: Int -> Int 
points numWinning = if numWinning == 0 then 0 else 2 ^ (numWinning - 1) 

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
solve1 = show . sum . map ( points . numMatches . parse) . lines


updateRest :: Int -> Int -> [Int] -> [Int]
updateRest 0 _ rem = rem
updateRest _ _ [] = []
updateRest i times (x : xs) = x + times : updateRest (i - 1) times xs



aggregateWithAcc :: [Int] -> [Int] -> [Int]
aggregateWithAcc acc [] = acc
aggregateWithAcc (accx: acccs ) (x : xs) =
   let newAccxs = updateRest x accx acccs
       res = aggregateWithAcc newAccxs xs
   in accx : res
   


aggregate :: [Int] -> [Int]
aggregate s = aggregateWithAcc [1 | i <- [1..(length s)]]  s

-- Approach:
-- First, we parse into a list of Cards,
-- Then we map the cards to a list of number of winners 
-- Then, we aggregate the list of numbers into our result 
-- We aggregate the numbers by having two lists, the list of original results &
-- A list of number of copies per card 
-- At the end, we sum up the number of copies
solve2 :: String -> String
solve2 = show . sum . aggregate . map (numMatches . parse ) . lines 
