import Data.Map (Map)
import qualified Data.Map as Map

main = do
  content <- readFile "input.txt"

  let linesOfInput = lines content
  let instructions = map parseInstruction linesOfInput
  let acc = findNonCyclical instructions 0
  print acc

parseInstruction :: String -> (String, Int)
parseInstruction line = (instruction, value)
  where lineWords = words line
        instruction = lineWords !! 0
        trimmedValue = dropWhile (\c -> c == '+') (lineWords !! 1)
        value = read trimmedValue :: Int

findNonCyclical :: [(String, Int)] -> Int -> Int
findNonCyclical instructions index
  | hasCycle == True = findNonCyclical instructions (index + 1)
  | otherwise = acc
  where (instruction, value) = instructions !! index
        swappedInstructions = swapInstructions instructions index
        (hasCycle, acc) = isCyclical swappedInstructions Map.empty 0 0

isCyclical :: [(String, Int)] -> Map Int Bool -> Int-> Int -> (Bool, Int)
isCyclical instructions visited index acc
  | index >= (length instructions) = (False, acc)
  | (Map.lookup index visited) == Nothing = isCyclical instructions newVisited newIndex newAcc
  | otherwise = (True, acc)
  where (newIndex, newAcc) = executeInstruction instructions index acc
        newVisited = Map.insert index True visited

executeInstruction :: [(String, Int)] -> Int -> Int -> (Int, Int)
executeInstruction instructions index acc
  | instruction == "nop" = (index + 1, acc)
  | instruction == "acc" = (index + 1, acc + value)
  | instruction == "jmp" = (index + value, acc)
  where (instruction, value) = instructions !! index

swapInstructions :: [(String, Int)] -> Int -> [(String, Int)]
swapInstructions instructions index
  | instruction == "acc" = instructions
  | instruction == "nop" = replaceElement instructions index ("jmp", value)
  | instruction == "jmp" = replaceElement instructions index ("nop", value)
  where (instruction, value) = instructions !! index

replaceElement :: [(String, Int)] -> Int -> (String, Int) -> [(String, Int)]
replaceElement instructions index newElement = take index instructions ++ newElement : drop (index + 1) instructions
