module Parser (parsePair) where

import Text.ParserCombinators.Parsec

hyphenatedPair :: GenParser Char st a -> GenParser Char st (a, a)
hyphenatedPair item = do
  first <- item
  char '-'
  second <- item
  return $ (first, second)

integer :: GenParser Char st Int
integer = do
  txt <- many digit
  return $ read txt

parsePair :: SourceName -> String -> Either ParseError (Int, Int)
parsePair = parse (hyphenatedPair integer)
