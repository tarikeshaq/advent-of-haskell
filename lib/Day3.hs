{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Day3 where
import Data.List(elemIndex)
import Data.Maybe(isJust)
import Debug.Trace(trace)

digits = ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0']

type Position = (Int, Int)

data NumItem = NumItem {
  num :: Int,
  startPosition :: Position,
  endPosition :: Position 
} deriving (Show)

unwrapNum :: NumItem -> Int
unwrapNum NumItem { num = num } = num

makeNumItem :: Int -> Position -> Position -> NumItem
makeNumItem num start end = NumItem {
  num = num,
  startPosition = start,
  endPosition = end
}

data Symbol = Symbol {
  symbol :: Char,
  position :: Position 
} deriving (Show)

makeSymbol :: Char -> Position -> Symbol
makeSymbol c p = Symbol {symbol = c, position = p  }


isNeighbor :: Symbol -> NumItem -> Bool
isNeighbor 
        Symbol {position = (symX, symY)} 
        NumItem { startPosition = (numSX, numSY), endPosition = (numEX, numEY)}
  | symY >= numSY - 1 && symY <= numSY + 1 = symX >= numSX - 1 && symX <= numEX + 1
  | otherwise = False


hasNeighbor :: [Symbol] -> NumItem -> Bool
hasNeighbor [] num = False
hasNeighbor (symbol : xs) num 
  | isNeighbor symbol num = True
  | otherwise = hasNeighbor xs num

filterNums :: ([NumItem], [Symbol]) -> [NumItem]
--filterNums (nums, symbols) | trace(show nums ++ show symbols) False = undefined
filterNums (nums, symbols) = filter (hasNeighbor symbols) nums

isDigit :: Char -> Bool
isDigit = isJust . flip elemIndex digits

parseNum :: String -> Position -> String -> (Int, Position, String)
parseNum (curr : xs) (x, y) acc
  | isDigit curr = parseNum xs (x+1, y) (curr : acc)
  | otherwise = (read $ reverse acc, (x, y), curr : xs) 

parseWithAcc :: String -> Position -> ([NumItem], [Symbol]) -> ([NumItem], [Symbol])
parseWithAcc [] _ acc = acc
parseWithAcc ('.' : xs) (x, y) acc = parseWithAcc xs (x+1, y) acc
parseWithAcc ('\n': xs) (x, y) acc = parseWithAcc xs (0, y + 1) acc
parseWithAcc (curr : xs) (x, y) (nums, symbols) 
  | isDigit curr = let (num, (newX, newY), rest) = parseNum (curr : xs) (x, y) ""
                  in parseWithAcc rest (newX, newY) ( makeNumItem num (x, y) (newX - 1, newY) : nums, symbols)
  | otherwise = parseWithAcc xs (x+1, y) (nums, makeSymbol curr (x, y) : symbols)

parse :: String -> ([NumItem], [Symbol])
parse s = parseWithAcc s (0, 0) ([], [])

-- Approach: We parse the input into a set of numbers and a set of 
-- symbols and their appropriate positions
-- Then, we filter the numbers to only include those who have an adjacent
-- symbol
-- Finally, we sum those numbers up

solve1 :: String -> String
solve1 = show . sum . map unwrapNum . filterNums . parse 



getAllNeighbors :: Symbol -> [NumItem] -> (Symbol, [NumItem])
getAllNeighbors sym = (sym,) . filter (isNeighbor sym) 

joinNeighborsWithAcc :: ([NumItem], [Symbol]) -> [(Symbol, [NumItem])] -> [(Symbol, [NumItem])]
joinNeighborsWithAcc (_, []) acc = acc
joinNeighborsWithAcc (nums, sym : xs) acc = 
    joinNeighborsWithAcc (nums, xs) (res : acc)
    where res = getAllNeighbors sym nums

joinNeighbors :: ([NumItem], [Symbol]) -> [(Symbol, [NumItem])]
joinNeighbors items = joinNeighborsWithAcc items [] 

starWithTwoNeighbors :: (Symbol, [NumItem]) -> Bool
starWithTwoNeighbors (Symbol { symbol = '*' }, nums) = length nums == 2
starWithTwoNeighbors _ = False 

multiplyNeighbors :: (Symbol, [NumItem]) -> Int
multiplyNeighbors (_, nums) = foldr (\curr acc -> case curr of 
                                       NumItem { num = n } -> n * acc) 1 nums

-- Approach: We parse the input the same way we parsed it in part 1
--   Then, we map the symbols and numbers to mapping between each symbol and all the symbols
--   neighbors
--   Then, we filter the symbols to only the ones with exatcly two neighbors
--   Then, we map the remaining symbols into the multiple of their neighbors
--   Finally, we sum those numbers up
solve2 :: String -> String
solve2 =
    show . sum . map multiplyNeighbors .
        filter starWithTwoNeighbors .
            joinNeighbors . parse 
