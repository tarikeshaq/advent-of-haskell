{-# LANGUAGE LambdaCase #-}

module Day2 where
import Debug.Trace(trace)

data SetItem = Red Int | Green Int | Blue Int | Other String deriving(Show)

type Set = [SetItem] 

data Game = Game {
  gameId :: Int,
  sets :: [Set]
} deriving (Show)

maxGreens = 13
maxBlues = 14
maxReds = 12

parseGameHelperNum :: String -> String -> (Int, String)
parseGameHelperNum (':' : ' ' : xs) num = (read num, xs)
parseGameHelperNum (x : xs) acc = parseGameHelperNum xs $ acc ++ [x] 

parseGameHelper :: String -> String -> (Int, String)
parseGameHelper rem "Game " = parseGameHelperNum rem "" 
parseGameHelper (x:xs) acc = parseGameHelper xs $ acc ++ [x]

parseGameId :: String -> (Int, String)
parseGameId s = parseGameHelper s "" 


parseSetNum :: String -> String -> (Int, String)
parseSetNum [] acc = (read acc, [])
parseSetNum (' ' : xs) curr = (read curr, xs)
parseSetNum (x : xs) acc = parseSetNum xs (acc ++ [x])

parseSetColor :: String -> String -> (String, String)
parseSetColor [] acc = (acc, [])
parseSetColor (',' : ' ' : xs) acc = (acc, xs)
parseSetColor (';' : ' ' : xs) acc = (acc, ';': ' ' : xs)
parseSetColor (x : xs) acc = parseSetColor xs (acc ++ [x])

parseSetHelper :: String -> [SetItem] -> (Set, String) 
parseSetHelper [] acc = (acc, [])
parseSetHelper (';': ' ' : xs) acc = (acc, xs) 
parseSetHelper s acc = 
  let 
    (num, colorRest) = parseSetNum s ""
    (color, rest) = parseSetColor colorRest ""
    item = case color of 
      "blue" -> Blue num 
      "red" -> Red num
      "green" -> Green num 
      foo -> Other foo
  in 
   parseSetHelper rest (item : acc)


parseSetsHelper :: String -> [Set] -> [Set]
parseSetsHelper [] res = res
parseSetsHelper s acc = 
  let (set, rest) = parseSetHelper s []
  in 
   parseSetsHelper rest $ set : acc

parseSets :: String -> [Set]
parseSets s = parseSetsHelper s []

parse :: String -> Game
parse s = 
   let
     (gameId, rest) = parseGameId s
     sets = parseSets rest
   in
    Game {
      gameId = gameId, 
      sets = sets
    }

validateSet :: Set -> Maybe ()
validateSet = 
   foldr (\x acc -> acc >> case x of 
          Green num -> if num > maxGreens then Nothing else Just ()
          Blue num -> if num > maxBlues then Nothing else Just ()
          Red num -> if num > maxReds then Nothing else Just ()
    ) $ Just ()


validateSets :: [Set] -> Maybe ()
validateSets = 
   foldr (\x acc -> acc >> validateSet x) $ Just () 

validate :: Game -> Maybe Int
-- validate g | trace(show g) False = undefined
validate Game { gameId = gameId, sets = sets} = validateSets sets >> Just gameId

solveGame :: String -> Int
solveGame = (\case
              Just a -> a
              Nothing -> 0) . validate . parse 


power :: (SetItem, SetItem, SetItem) -> Int
power (Red red, Green green, Blue blue) = red * green * blue

maxColorVals :: Set -> (SetItem, SetItem, SetItem) -> (SetItem, SetItem, SetItem)
maxColorVals set acc = 
   foldl (\curr x -> case (x, curr) of
          (Green num, (red, Green greenNum, blue)) -> (red, Green (max num greenNum), blue) 
          (Blue num, (red, green, Blue blueNum)) -> (red, green, Blue (max num blueNum))
          (Red num, (Red redNum, green, blue)) -> (Red (max num redNum), green, blue)
          (_, curr) -> curr
     ) acc set
  
getMinNeededHelper :: [Set] -> (SetItem, SetItem, SetItem)
getMinNeededHelper = 
   foldl (flip maxColorVals) (Red 0, Green 0, Blue 0)

getMinNeeded :: Game -> (SetItem, SetItem, SetItem)
getMinNeeded Game { sets = sets } = 
   getMinNeededHelper sets 

solveGame2 :: String -> Int
solveGame2 = power . getMinNeeded . parse

solve1 :: String -> String
solve1 = show . sum . map solveGame . lines

solve2 :: String -> String
solve2 = show . sum . map solveGame2 . lines
