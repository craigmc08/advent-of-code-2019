import Eval
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.Maybe (isJust, fromJust)
import Data.List (intercalate)

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split c xs = let (begin, end) = span (/=c) xs
             in begin : (split c $ drop 1 end)

main = do
  args <- getArgs
  input <- if length args > 0 then readFile (head args) else putStr "Enter code> " >> getLine
  let code = setVal 1 12 $ setVal 2 2 $ map read $ split ',' input
  let soln = eval code
  case soln of
    Right xs -> putStrLn $ intercalate "," $ map show xs
    Left err -> putStrLn err
  
