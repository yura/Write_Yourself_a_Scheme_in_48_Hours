module Lib
    ( symbol, readExpr, LispVal(..), parseString, escapedChars, readBin, parseNumber
    ) where

import Control.Monad
import Data.Char (isDigit, digitToInt)
import Numeric (readInt, readOct, readHex)
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
   Number x == Number y = x == y

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapedChars = do char '\\'
                  x <- oneOf "\\\"nrt"
                  return $ case x of
                    '\\' -> x
                    '"' -> x
                    'n'  -> '\n'
                    'r'  -> '\r'
                    't'  -> '\t'

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ escapedChars <|> noneOf "\"\\"
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

readBin :: String -> Integer
readBin = (fst . head . readInt 2 isDigit digitToInt)

parseNumberWithPrefix :: Parser LispVal
parseNumberWithPrefix = do
  char '#'
  prefix <- oneOf "bodx"
  x <- many1 (digit <|> oneOf "abcdef") -- #FIXME: x can have letters when it is in hex format only
  (return . Number) $ case prefix of
    'b' -> readBin x
    'o' -> (fst . head . readOct) x
    'd' -> read x
    'x' -> (fst . head . readHex) x

parseNumberWithoutPrefix :: Parser LispVal
parseNumberWithoutPrefix = do
  x <- many1 digit
  (return . Number . read) x

-- using liftM
--parseNumber = liftM (Number . read) $ many1 digit

-- Using >>= (bind) operator
--parseNumber = (many1 digit) >>= return . Number . read

-- using do-notation
parseNumber :: Parser LispVal
parseNumber =
  parseNumberWithPrefix <|> parseNumberWithoutPrefix

parseExpr :: Parser LispVal
parseExpr = parseString
        <|> parseAtom
        <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Value found: " ++ show val

