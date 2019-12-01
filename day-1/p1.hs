import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.Maybe (isJust, fromJust)
import Control.Monad (guard)

requiredFuel :: Integer -> Integer
requiredFuel m = m `div` 3 - 2

main = do
  args <- getArgs
  guard $ length args > 0
  let filename = head args
  input <- readFile filename
  let soln = sum $ map (requiredFuel . fromJust) $ filter isJust $ map readMaybe $ lines input
  print soln
