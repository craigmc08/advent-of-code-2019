module Move (parseWires, Move (..), Wire, manhattan, tracePath, findIntersections) where

import Text.ParserCombinators.Parsec
import Data.List (intersect)

data Move = U Integer | D Integer | L Integer | R Integer deriving Show

type Wire = [Move]

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

manhattan :: (Integer, Integer) -> (Integer, Integer) -> Integer
manhattan (x,y) (i,j) = abs $ x - i + y - j

tracePath :: (Integer, Integer) -> Wire -> [(Integer, Integer)]
tracePath point [] = []
tracePath (x, y) (U n:ms) = let ps = [(x, y + i) | j <- [0..n - 1], let i = n - j] in ps ++ tracePath (head ps) ms
tracePath (x, y) (D n:ms) = let ps = [(x, y - i) | j <- [0..n - 1], let i = n - j] in ps ++ tracePath (head ps) ms
tracePath (x, y) (L n:ms) = let ps = [(x - i, y) | j <- [0..n - 1], let i = n - j] in ps ++ tracePath (head ps) ms
tracePath (x, y) (R n:ms) = let ps = [(x + i, y) | j <- [0..n - 1], let i = n - j] in ps ++ tracePath (head ps) ms

findIntersections :: Wire -> Wire -> [(Integer, Integer)]
findIntersections w1 w2 = intersect (tracePath (0, 0) w1) (tracePath (0, 0) w2)
