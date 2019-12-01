import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.Maybe (isJust, fromJust)
import Control.Monad (guard)

requiredFuelStep :: Integer -> Integer
requiredFuelStep m = m `div` 3 - 2

requiredFuel :: Integer -> Integer
requiredFuel init = let amounts = init : [requiredFuelStep $ amounts !! i | i <- [0..]]
                    in  sum $ drop 1 $ takeWhile (>0) amounts

main = do
  args <- getArgs
  guard $ length args > 0
  let filename = head args
  input <- readFile filename
  let soln = sum $ map (requiredFuel . fromJust) $ filter isJust $ map readMaybe $ lines input
  print soln
