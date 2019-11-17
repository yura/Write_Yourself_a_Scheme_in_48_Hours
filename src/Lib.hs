module Lib
    ( symbol, readExpr, LispVal(..), parseBool, parseCharacter, parseString, escapedChars
    , readBin
    , parseBin
    , parseOct
    , parseDecimal
    , parseDecimalWithPrefix
    , parseHex
    , parseNumber
    , parseExpr
    ) where

import Control.Monad
import Data.Char (isDigit, digitToInt)
import Numeric (readInt, readOct, readHex)
import Text.Parsec.Char
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | Character Char
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
  show (Character a) = show a

instance Eq LispVal where
   String x == String y = x == y
   Number x == Number y = x == y
   Bool x == Bool y = x == y
   Character x == Character y = x == y

-- using liftM
--parseNumber = liftM (Number . read) $ many1 digit

-- Using >>= (bind) operator
--parseNumber = (many1 digit) >>= return . Number . read

-- using do-notation
-- parseNumber = do x <- many1 digit
--                 (return . Number . read) x

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseBool :: Parser LispVal
parseBool = do
  char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseCharacter :: Parser LispVal
parseCharacter = do
  try $ string "#\\"
  value <- try (string "space" <|> string "newline")
          <|> do
                 x <- anyChar; notFollowedBy alphaNum; return [x]
  return $ Character $ case value of
    "space"   -> ' '
    "newline" -> '\n'
    otherwise -> (value !! 0)

parseDecimal :: Parser LispVal
parseDecimal = many1 digit >>= (return . Number . read)

parseDecimalWithPrefix :: Parser LispVal
parseDecimalWithPrefix = do
  try (string "#d")
  many1 digit >>= (return . Number . read)

readBin :: String -> Integer
readBin = (fst . head . readInt 2 isDigit digitToInt)

parseBin :: Parser LispVal
parseBin = do
  try $ string "#b"
  many1 (oneOf "01") >>= return . Number . readBin

parseOct :: Parser LispVal
parseOct = do
  try $ string "#o"
  many1 octDigit >>= return . Number . fst . head . readOct

parseHex :: Parser LispVal
parseHex = do
  try $ string "#x"
  many hexDigit >>= return . Number . fst . head . readHex

parseNumber :: Parser LispVal
parseNumber = parseDecimal <|> parseDecimalWithPrefix <|> parseBin <|> parseOct <|> parseHex

escapedChars = do char '\\'
                  x <- oneOf "\\\"nrt"
                  return $ case x of
                    '\\' -> x
                    '"' -> x
                    'n'  -> '\n'
                    'r'  -> '\r'
                    't'  -> '\t'

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    _    -> Atom atom

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ escapedChars <|> noneOf "\"\\"
  char '"'
  return $ String x

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseNumber
        <|> try parseBool
        <|> try parseCharacter

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Value found: " ++ show val

