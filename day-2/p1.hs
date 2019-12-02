import Eval
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.Maybe (isJust, fromJust)
import Data.List (intercalate)

main = do
  args <- getArgs
  input <- if length args > 0 then readFile (head args) else putStr "Enter code> " >> getLine
  let soln = evalStr (eval . setVal 1 12 . setVal 2 2) input
  case soln of
    Right xs -> putStrLn $ intercalate "," $ map show xs
    Left err -> putStrLn err
  
