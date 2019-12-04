module Move (parseWires, Move (..), Wire, Pos, manhattan, tracePath, findIntersections) where

import Text.ParserCombinators.Parsec
import Data.List (intersectBy)

data Move = U Integer | D Integer | L Integer | R Integer deriving Show

type Wire = [Move]

type Pos = (Integer, Integer)

wirePair :: GenParser Char st (Wire, Wire)
wirePair = do
  fstList <- wire
  eol
  sndList <- wire
  eol
  return (fstList, sndList)

wire :: GenParser Char st Wire
wire = move `sepBy` char ','

move :: GenParser Char st Move
move = do
  dir <- oneOf "UDLR"
  let constructor = case dir of
                    'U' -> U
                    'D' -> D
                    'L' -> L
                    'R' -> R
  digits <- many digit
  let num = read digits
  return $ constructor num

eol :: GenParser Char st String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

parseWires :: SourceName -> String -> Either ParseError (Wire, Wire)
parseWires = parse wirePair

manhattan :: Pos -> Pos -> Integer
manhattan (x,y) (i,j) = abs $ x - i + y - j

makeSteps :: Integer -> Integer -> [Integer]
makeSteps s n = [s + (n - i) | i <- [0..n-1]]

tracePath :: Wire -> Integer -> Pos-> [(Pos, Integer)]
tracePath [] _ _ = []
tracePath (U n:ms) s (x, y) = let ps = [(x, y + i) | j <- [0..n - 1], let i = n - j] in zip ps (makeSteps s n) ++ tracePath ms (s + n) (head ps)
tracePath (D n:ms) s (x, y) = let ps = [(x, y - i) | j <- [0..n - 1], let i = n - j] in zip ps (makeSteps s n) ++ tracePath ms (s + n) (head ps)
tracePath (L n:ms) s (x, y) = let ps = [(x - i, y) | j <- [0..n - 1], let i = n - j] in zip ps (makeSteps s n) ++ tracePath ms (s + n) (head ps)
tracePath (R n:ms) s (x, y) = let ps = [(x + i, y) | j <- [0..n - 1], let i = n - j] in zip ps (makeSteps s n) ++ tracePath ms (s + n) (head ps)

intersectPairs :: (a -> a -> Bool) -> [a] -> [a] -> [(a, a)]
intersectPairs _ [] _  = []
intersectPairs _ _  [] = []
intersectPairs p xs ys = [(x, y) | x <- xs, y <- filter (p x) ys]

findIntersections :: Wire -> Wire -> [((Pos, Integer), (Pos, Integer))]
findIntersections w1 w2 = let p1 = tracePath w1 0 (0, 0)
                              p2 = tracePath w2 0 (0, 0)
                          in intersectPairs (\m n -> fst m == fst n) p1 p2
