module Main where
import System.Environment

import Day1
import Day2
import Day3
import Day4

data Day = Day {
  input:: String,
  solver:: String -> String
}


getSolveFn :: String -> String -> String -> String
getSolveFn "1" "1" = Day1.solve1 
getSolveFn "1" "2" = Day1.solve2
getSolveFn "2" "1" = Day2.solve1
getSolveFn "2" "2" = Day2.solve2
getSolveFn "3" "1" = Day3.solve1 
getSolveFn "3" "2" = Day3.solve2 
getSolveFn "4" "1" = Day4.solve1 
getSolveFn "4" "2" = Day4.solve2 

makeInputFileName :: String -> String
--makeInputFileName a = "input/" ++ "Day" ++ a ++ ".txt"  
makeInputFileName a = "input/Day4-test.txt"
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
