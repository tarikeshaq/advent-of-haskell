module Main where
import System.Environment

import Day1

data Day = Day {
  input:: String,
  solver:: String -> String
}


getSolveFn :: String -> String -> String -> String
getSolveFn "1" "1" = Day1.solve1 
getSolveFn "1" "2" = Day1.solve2


makeInputFileName :: String -> String
makeInputFileName a = "input/" ++ "Day" ++ a ++ ".txt"  
-- makeInputFileName a = "input/Day1-test.txt"

getSolver :: [String] -> IO Day
getSolver (day : part : _) = 
  let filename = makeInputFileName day 
   in 
     readFile filename >>=
       \x ->
        return Day {
           input = x,
           solver = getSolveFn day part
        }
   
solve :: Day -> IO String
solve (Day { input = i, solver = s }) = 
  return $ s i 

main :: IO ()
main = getArgs >>= getSolver >>= solve >>= putStrLn 
