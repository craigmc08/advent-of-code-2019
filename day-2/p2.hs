-- Really lazy solution, but whatever
import Eval
import System.Environment (getArgs)
import Data.List (intercalate, find)
import Data.Either (isRight, fromRight)

findInputs :: (([Int] -> Either String [Int]) -> Either String [Int]) -> Maybe (Int, Int)
findInputs evalrr = let cases = [ (noun, verb) | noun <- [0..99], verb <- [0..99] ]
                        tests = [ eval . setVal 1 noun . setVal 2 verb | (noun, verb) <- cases ]
                        solns = zip cases $ map evalrr tests
                    in  fst <$> (find ((==19690720) . head . fromRight [0] . snd) $ filter (isRight . snd) $ solns)

main = do
  args <- getArgs
  input <- if length args > 0 then readFile (head args) else putStr "Enter code> " >> getLine
  let soln = findInputs $ flip evalStr input
  case soln of
    Just (noun, verb) -> putStrLn $ "noun=" ++ show noun ++ ", verb=" ++ show verb ++ ", solution=" ++ show (100 * noun + verb)
    Nothing -> putStrLn "No solutions found"
  