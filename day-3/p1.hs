{-# LANGUAGE TupleSections #-}

import System.Environment (getArgs)
import Move

main :: IO ()
main = do
  args <- getArgs
  (src, input) <- case length args of
                    0 -> getContents >>= return . ("stdin",)
                    _ -> (readFile $ head args) >>= return . (head args,)
  case parseWires "" input of
    Right wires -> 
      do
        print $ minimum $ filter (/=0) $ map (manhattan (0, 0)) $ uncurry findIntersections wires
    Left e -> print e
