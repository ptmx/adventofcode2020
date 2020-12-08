import Data.Map (Map)
import qualified Data.Map as Map

main = do
  content <- readFile "input.txt"

  let linesOfInput = lines content
  let instructions = map parseInstruction linesOfInput
  let acc = getAccBeforeCycle instructions Map.empty 0 0
  print acc

parseInstruction :: String -> (String, Int)
parseInstruction line = (instruction, value)
  where lineWords = words line
        instruction = lineWords !! 0
        trimmedValue = dropWhile (\c -> c == '+') (lineWords !! 1)
        value = read trimmedValue :: Int

getAccBeforeCycle :: [(String, Int)] -> Map Int Bool -> Int-> Int -> Int
getAccBeforeCycle instructions visited index acc
  | (Map.lookup index visited) == Nothing = getAccBeforeCycle instructions newVisited newIndex newAcc
  | otherwise = acc
  where (newIndex, newAcc) = executeInstruction instructions index acc
        newVisited = Map.insert index True visited

executeInstruction :: [(String, Int)] -> Int -> Int -> (Int, Int)
executeInstruction instructions index acc
  | instruction == "nop" = (index + 1, acc)
  | instruction == "acc" = (index + 1, acc + value)
  | instruction == "jmp" = (index + value, acc)
  where (instruction, value) = instructions !! index
