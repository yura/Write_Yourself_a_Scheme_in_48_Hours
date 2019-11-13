module Lib
    ( symbol, readExpr, LispVal(..), parseString
    ) where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where
  show (Atom a) = show a
  show (List a) = show a
  show (Number a) = show a
  show (String a) = show a
  show (Bool a) = show a

instance Eq LispVal where
   String x == String y = x == y

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ (char '\\' >> char '"') <|> noneOf "\"\\"
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
-- using liftM
--parseNumber = liftM (Number . read) $ many1 digit

-- using do-notation
--parseNumber = do
--  x <- many1 digit
--  (return . Number . read) x

-- Using >>= (bind) operator
parseNumber = (many1 digit) >>= return . Number . read

parseExpr :: Parser LispVal
parseExpr = parseString
        <|> parseAtom
        <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match" ++ show err
  Right val -> "Value found"

