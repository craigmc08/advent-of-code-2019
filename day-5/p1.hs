{-# LANGUAGE TupleSections #-}

import System.Environment (getArgs)
import IntCode

main :: IO ()
main = do
  args <- getArgs
  (src, input) <- case length args of
                    0 -> putStr "Enter IntCode Program> " >> getLine >>= return . ("stdin",)
                    _ -> (readFile $ head args) >>= return . (head args,)
  evalProgram input
  return ()
