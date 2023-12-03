module Day1 where
import Debug.Trace(trace)
import Data.List(elemIndex)

numDigits = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]
numStrs = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"
           ,"one", "two", "three", "four", "five", "six", "seven", "eight",
           "nine", "zero"]
numVals = [1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0]

myIndex :: (Show a) => [a] -> Int -> Maybe a 
myIndex [] _ = Nothing 
myIndex (x: _) 0 = Just x
myIndex (_ : xs) i = myIndex xs (i - 1)


toNum :: [String] -> String -> Maybe Int 
toNum numList str = elemIndex str numList >>= myIndex numVals 

applyToSubstrings :: Char -> [String] -> [String] -> [String]
applyToSubstrings a [] acc = acc ++ [[a]] 
applyToSubstrings a (x : xs) acc = applyToSubstrings a xs (acc ++ [a : x]) 

substringsHelper :: Char -> String -> ([String], [String])
substringsHelper curr [] = ([[curr]], [[curr]])
substringsHelper curr (x:xs) = 
   case substringsHelper x xs of
   (nextSublist, allSubstrings) -> 
     let 
       mySublist = applyToSubstrings curr nextSublist []
     in 
      (mySublist, mySublist ++ allSubstrings)

substrings :: String -> [String]
substrings [] = []
substrings (x : xs) = case substringsHelper x xs of  (_, all) -> all
 
foldrSubstrs :: (String -> a -> a) -> a -> String -> a
foldrSubstrs f acc str = foldr f acc $ substrings str

foldlSubstrs :: (a -> String -> a) -> a -> String -> a
foldlSubstrs f acc str = foldl f acc $ substrings str 

accumlateNum :: [String] -> Maybe Int -> String -> Maybe Int
accumlateNum _ (Just res) _ = Just res
accumlateNum numList Nothing curr = toNum numList curr

getFirstNum ::  [String] -> String -> Maybe Int 
getFirstNum numList = foldlSubstrs (accumlateNum numList)  Nothing

getLastNum :: [String] -> String -> Maybe Int 
getLastNum numList = foldrSubstrs (flip $ accumlateNum numList) Nothing

nums :: [String] -> String -> Int 
nums numList s = case (getFirstNum numList s, getLastNum numList s) of 
  (Just a, Just b) -> (a * 10) + b
  _ -> 0 

solve1 :: String -> String
solve1 = show . sum . map (nums numDigits) . lines

solve2 :: String -> String 
solve2 = show . sum . map (nums numStrs) . lines
