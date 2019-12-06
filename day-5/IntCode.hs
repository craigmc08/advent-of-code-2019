module IntCode (evalProgram, parseProgram) where

import Text.ParserCombinators.Parsec
import Data.Char (digitToInt)
import Data.Either (isRight)
import Control.Applicative (liftA2, liftA3)

data EvalState = Success Int [Int] | Fail String | Stop [Int] deriving Show

eitherToEval :: Int -> Either String [Int] -> EvalState
eitherToEval _ (Left reason) = Fail reason
eitherToEval ptr (Right mem) = Success ptr mem

incrementPtr :: Int -> EvalState -> EvalState
incrementPtr _ (Stop mem) = Stop mem
incrementPtr _ (Fail reason) = Fail reason
incrementPtr n (Success ptr mem) = Success (ptr + n) mem

data Op = Add       Int Int Int Int Int Int 
        | Mul       Int Int Int Int Int Int
        | Input     Int         Int
        | Output    Int         Int
        | JumpTrue  Int Int     Int Int
        | JumpFalse Int Int     Int Int
        | LessThan  Int Int Int Int Int Int
        | Equals    Int Int Int Int Int Int
        | Halt
        deriving Show

digitsOf :: Int -> [Int]
digitsOf = map digitToInt . show

undigitsOf :: [Int] -> Int
undigitsOf = sum . zipWith (*) [10^n | n <- [0..]] . reverse

leftPad :: Int -> a -> [a] -> [a]
leftPad n c = until ((==n) . length) (c:)

eval :: EvalState -> IO EvalState
eval (Stop mem) = return $ Stop mem
eval (Fail reason) = return $ Fail reason
eval s@(Success ptr mem) = do
  let op' = getOp s
  case op' of
    Left reason -> return $ Fail reason
    Right op -> do
      s' <- evalOp op s
      eval s'


createOp :: Int -> [Int] -> [Int] -> Either String Op
createOp 1 (m:n:q:[]) (l:r:o:[]) = Right $ Add m n q l r o
createOp 1 _ _ = Left "Wrong number of argument modes or argument values to add instruction"

createOp 2 (m:n:q:[]) (l:r:o:[]) = Right $ Mul m n q l r o
createOp 2 _ _ = Left "Wrong number of argument modes or argument values to multiply instruction"

createOp 3 (m:[]) (o:[]) = Right $ Input m o
createOp 3 _ _ = Left "Wrong number of argument modes or argument values to input instruction"

createOp 4 (m:[]) (i:[]) = Right $ Output m i
createOp 4 _ _ = Left "Wrong number of argument modes or argument values to output instruction"

createOp 5 (m:n:[]) (c:o:[]) = Right $ JumpTrue m n c o
createOp 5 _ _ = Left "Wrong number of argument modes or argument values to jump-if-true instruction"

createOp 6 (m:n:[]) (c:o:[]) = Right $ JumpFalse m n c o
createOp 6 _ _ = Left "Wrong number of argument modes or argument values to jump-if-false instruction"

createOp 7 (m:n:q:[]) (l:r:o:[]) = Right $ LessThan m n q l r o
createOp 7 _ _ = Left "Wrong number of argument modes or argument values to less-than instruction"

createOp 8 (m:n:q:[]) (l:r:o:[]) = Right $ Equals m n q l r o
createOp 8 _ _ = Left "Wrong number of argument modes or argument values to equals instruction"

createOp 99 [] [] = Right $ Halt
createOp 99 _ _ = Left "Wrong number of argument modes or argument values to halt instruction"

createOp _ _ _ = Left "Unknown instruction"

getOp :: EvalState -> Either String Op
getOp (Stop mem) = Left "Attempted to acquire next instruction after program halted"
getOp (Fail reason) = Left reason
getOp (Success ptr mem) 
  | ptr >= length mem = Left "Pointer outside of program memory"
  | otherwise         = let codeDigits = digitsOf $ mem !! ptr
                            (unpaddedArgModes, opcode') = splitAt (length codeDigits - 2) codeDigits
                            opcode = undigitsOf opcode'
                            numArgs = numArgsFor opcode
                            argModes = reverse $ leftPad numArgs 0 unpaddedArgModes
                            argVals = take numArgs $ drop (ptr + 1) mem
                        in  createOp opcode argModes argVals
  
numArgsFor :: Int -> Int
numArgsFor 1 = 3 -- Add
numArgsFor 2 = 3 -- Multiply
numArgsFor 3 = 1 -- Input
numArgsFor 4 = 1 -- Output
numArgsFor 5 = 2
numArgsFor 6 = 2
numArgsFor 7 = 3
numArgsFor 8 = 3
numArgsFor 99 = 0 -- Halt
numArgsFor _ = 0 -- Unknown

-- numArgsOf :: Op -> Int
-- numArgsOf (Add _ _ _ _ _ _) = 3
-- numArgsOf (Mul _ _ _ _ _ _) = 3
-- numArgsOf (Input _ _) = 1
-- numArgsOf (Output _ _) = 1
-- numArgsOf (JumpTrue _ _ _ _) = 2
-- numArgsOf (JumpFalse _ _ _ _) = 2
-- numArgsOf (LessThan _ _ _ _ _ _) = 3
-- numArgsOf (Equals _ _ _ _ _ _) = 3
-- numArgsOf Halt = 0

evalOp :: Op -> EvalState -> IO EvalState
evalOp _ (Stop mem) = return $ Stop mem
evalOp _ (Fail reason) = return $ Fail reason

evalOp (Add lm rm om lv rv ov) (Success ptr mem) =
  let l = resolveArg lm lv mem
      r = resolveArg rm rv mem
      o = Right ov
  in return $ eitherToEval (ptr + 4) $ setValL o (liftA2 (+) l r) (Right mem)

evalOp (Mul lm rm om lv rv ov) (Success ptr mem) =
  let l = resolveArg lm lv mem
      r = resolveArg rm rv mem
      o = Right ov
  in return $ eitherToEval (ptr + 4) $ setValL o (liftA2 (*) l r) (Right mem)

evalOp (Input om ov) (Success ptr mem) =
  let o = Right ov
  in  do
        v <- putStr "> " >> getLine
        return $ eitherToEval (ptr + 2) $ setValL o (Right $ read v) (Right mem)

evalOp (Output im iv) (Success ptr mem) =
  case resolveArg im iv mem of
    Left e -> return $ Fail e
    Right i -> print i >> (return $ Success (ptr + 2) mem)

evalOp (JumpTrue cm jm cv jv) (Success ptr mem) =
  let c = resolveArg cm cv mem
      j = resolveArg jm jv mem
  in  if either (const 0) id c /= 0
        then return $ Success (either (const ptr) id j) mem
        else return $ Success (ptr + 3) mem

evalOp (JumpFalse cm jm cv jv) (Success ptr mem) =
  let c = resolveArg cm cv mem
      j = resolveArg jm jv mem
  in  if either (const 1) id c == 0
        then return $ Success (either (const ptr) id j) mem
        else return $ Success (ptr + 3) mem

evalOp (LessThan lm rm _ lv rv ov) (Success ptr mem) =
  let l = resolveArg lm lv mem
      r = resolveArg rm rv mem
      o = Right ov
  in  return $ eitherToEval (ptr + 4) $ setValL o (Right $ if either (const False) id (liftA2 (<) l r) then 1 else 0) (Right mem)

evalOp (Equals lm rm _ lv rv ov) (Success ptr mem) =
  let l = resolveArg lm lv mem
      r = resolveArg rm rv mem
      o = Right ov
  in  return $ eitherToEval (ptr + 4) $ setValL o (Right $ if either (const False) id (liftA2 (==) l r) then 1 else 0) (Right mem)

evalOp Halt (Success ptr mem) = return $ Stop mem

resolveArg :: Int -> Int -> [Int] -> Either String Int
resolveArg 0 ptr mem -- Position arg mode
  | ptr >= length mem = Left "Argument value pointer outside of program memory"
  | otherwise         = Right $ mem !! ptr

resolveArg 1 val _ = Right val -- Immediate arg mode

resolveArg _ _ _ = Right 0 -- Unknown arg mode

setVal :: Int -> a -> [a] -> [a]
setVal n x xs = take n xs ++ [x] ++ drop (n + 1) xs

setValL :: (Applicative f) => f Int -> f a -> f [a] -> f [a]
setValL = liftA3 setVal

-- https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/parsing-floats-with-parsec
int :: GenParser Char st Int
int = read <$> (plus <|> minus <|> number)
  where plus = char '+' *> number
        minus = (:) <$> char '-' <*> number
        number = many1 digit

ints :: GenParser Char st [Int]
ints = int `sepBy` (char ',')

parseProgram :: String -> Either String [Int]
parseProgram = either (Left . show) Right . parse ints "(unknown)"

initProgram :: String -> EvalState
initProgram src = case parseProgram src of
                    Left reason -> Fail reason
                    Right mem -> Success 0 mem

evalProgram :: String -> IO EvalState
evalProgram = eval . initProgram
