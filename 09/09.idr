module Main

import Data.SortedMap
import Data.String

castInteger : String -> Integer
castInteger s =
  case parsed of
    Just val => val
    Nothing  => 0
  where
    parsed : Maybe Integer
    parsed = parseInteger s

enumerate : List a -> Nat -> List (a, Nat)
enumerate [] _      = []
enumerate (x::xs) n = [(x, n)] ++ enumerate xs (S n)

lookupOrDefault : SortedMap k v -> k -> v -> v
lookupOrDefault sortedMap key defaultValue =
  case value of
    Nothing  => defaultValue
    Just val => val
  where value : Maybe v
        value = SortedMap.lookup key sortedMap

headOrDefault : List a -> a -> a 
headOrDefault list default =
  case element of
    Nothing => default
    Just el => el
  where element : Maybe a 
        element = head' list

lastOrDefault : List a -> a -> a 
lastOrDefault list default =
  case element of
    Nothing => default
    Just el => el
  where element : Maybe a 
        element = last' list

indexOrDefault : List a -> Nat -> a -> a
indexOrDefault list idx default =
  case element of
    Nothing => default
    Just el => el
  where element : Maybe a
        element = index' idx list

mapIndexes : List (Integer, Nat) -> SortedMap Integer (List Nat) -> SortedMap Integer (List Nat)
mapIndexes [] indexes = indexes
mapIndexes (x::xs) indexes = mapIndexes xs (SortedMap.insert int (currentIndexes ++ [idx]) indexes)
  where int : Integer
        int = fst x
        idx : Nat
        idx = snd x
        currentIndexes : List Nat
        currentIndexes = lookupOrDefault indexes int []

isValidSum : List Integer -> SortedMap Integer (List Nat) -> Nat -> Nat -> Bool
isValidSum integers indexes startIndex endIndex =
  any (\i => i > startIndex && i < endIndex) matchingIndexes
  where startElement : Integer
        startElement = indexOrDefault integers startIndex 0
        endElement : Integer
        endElement = indexOrDefault integers endIndex 0
        difference : Integer
        difference = endElement - startElement
        matchingIndexes : List Nat
        matchingIndexes = lookupOrDefault indexes difference []

isValid : List Integer -> SortedMap Integer (List Nat) -> Nat -> Nat -> Bool
isValid integers indexes startIndex endIndex =
  case startIndex >= endIndex of
    True  => False
    False => case isCurrentIndexValid of
      True  => True
      False => isValid integers indexes (startIndex + 1) endIndex
  where isCurrentIndexValid : Bool
        isCurrentIndexValid = isValidSum integers indexes startIndex endIndex

findFirstInvalid : List Integer -> SortedMap Integer (List Nat) -> Nat -> Nat -> Maybe Integer
findFirstInvalid integers indexes preambleLength idx =
  case isCurrentValid of
    False => index' (idx + preambleLength) integers
    True  => findFirstInvalid integers indexes preambleLength (idx + 1)
  where isCurrentValid : Bool
        isCurrentValid = isValid integers indexes idx (idx + preambleLength)

findMinMax : List Integer -> (Integer, Integer)
findMinMax integers = (smallest, largest)
  where sorted : List Integer
        sorted = sort integers
        smallest : Integer
        smallest = headOrDefault sorted 0
        largest : Integer
        largest = lastOrDefault sorted 0

sumMinMax : List Integer -> Integer
sumMinMax integers = (fst minMax) + (snd minMax)
  where minMax : (Integer, Integer)
        minMax = findMinMax integers 

checkSumsStartingFromIndex : List Integer -> Integer -> Nat -> Nat -> Integer -> Maybe (List Integer)
checkSumsStartingFromIndex integers target startIndex offset sum =
  case ((currentIndex >= (length integers)) || (summed > target)) of
    True  => Nothing
    False => case (summed == target) of
      True  => Just elements
      False => checkSumsStartingFromIndex integers target startIndex (offset + 1) summed
  where currentIndex : Nat
        currentIndex = startIndex + offset 
        elements : List Integer
        elements = take (offset + 1) (drop startIndex integers)
        summed : Integer
        summed = sum + (indexOrDefault integers currentIndex 0)

findContiguousSum : List Integer -> Integer -> Nat -> Maybe (List Integer)
findContiguousSum integers target startIndex =
  case (startIndex >= (length integers)) of
    True  => Nothing
    False => case foundSum of
      Nothing => findContiguousSum integers target (startIndex + 1)
      Just xs => Just xs
  where foundSum : Maybe (List Integer)
        foundSum = checkSumsStartingFromIndex integers target startIndex 1 (indexOrDefault integers startIndex 0)

solveA : String -> Maybe Integer
solveA input = findFirstInvalid integers indexes 25 0
  where ws : List String
        ws = words input
        integers : List Integer
        integers = map castInteger ws
        enumerated : List (Integer, Nat)
        enumerated = enumerate integers Z
        indexes : SortedMap Integer (List Nat)
        indexes = mapIndexes enumerated SortedMap.empty

solveB : String -> Integer -> Integer
solveB input target =
  case contiguousSum of
    Nothing => -1
    Just xs => sumMinMax xs
  where ws : List String
        ws = words input
        integers : List Integer
        integers = map castInteger ws
        contiguousSum : Maybe (List Integer)
        contiguousSum = findContiguousSum integers target 0

solve : String -> Maybe (Integer, Integer)
solve input =
  case solutionA of
    Nothing     => Nothing
    Just target => Just (target, (solveB input target))
  where solutionA : Maybe Integer
        solutionA = solveA input

main : IO ()
main = do
  file <- readFile "input.txt"
  case file of
    Right input => print (solve input)
    Left err => printLn err
