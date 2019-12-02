module Eval where

setVal :: Int -> a -> [a] -> [a]
setVal n x xs = take n xs ++ [x] ++ drop (n + 1) xs

evalOp :: (Int -> Int -> Int) -> Int -> [Int] -> Either String [Int]
evalOp op p xs = let l = xs !! (p + 1)
                     r = xs !! (p + 2)
                     o = xs !! (p + 3)
                 in  eval' (p + 4) $ setVal o (op (xs !! l) (xs !! r)) xs

eval' :: Int -> [Int] -> Either String [Int]
eval' p xs = case xs !! p of
               1 -> evalOp (+) p xs
               2 -> evalOp (*) p xs
               99 -> Right xs
               op -> Left $ "Unknown opcode " ++ (show op)

eval :: [Int] -> Either String [Int]
eval = eval' 0
