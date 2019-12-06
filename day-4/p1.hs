{-# LANGUAGE TupleSections #-}

import System.Environment (getArgs)
import Data.Char (digitToInt)
import Parser

digits :: Int -> [Int]
digits = map digitToInt . show

isIncreasing :: (Ord a) => [a] -> Bool
isIncreasing []       = True
isIncreasing (x:[])   = True
isIncreasing (x:y:xs) = y >= x && isIncreasing (y:xs)

hasAdjacentDuplicates :: (Eq a) => [a] -> Bool
hasAdjacentDuplicates []       = False
hasAdjacentDuplicates (x:[])   = False
hasAdjacentDuplicates (x:y:xs) = if x == y then True
                                 else hasAdjacentDuplicates (y:xs)


validCombination :: Int -> Bool
validCombination n = let dgts = digits n
                     in  length dgts == 6 && isIncreasing dgts && hasAdjacentDuplicates dgts

main :: IO ()
main = do
  args <- getArgs
  (src, input) <- case length args of
                    0 -> putStr "Enter Pair> " >> getLine >>= return . ("stdin",)
                    _ -> (readFile $ head args) >>= return . (head args,)
  case parsePair src input of
    Left e -> print e
    Right (low, high) -> print $ length $ filter validCombination $ [low..high]
