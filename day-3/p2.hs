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
        -- sequence $ map print $ tracePath (fst wires) 0 (0, 0)
        -- sequence $ map print $ uncurry findIntersections wires
        print $ minimum $ map (\(a, b) -> snd a + snd b) $ uncurry findIntersections wires
    Left e -> print e
