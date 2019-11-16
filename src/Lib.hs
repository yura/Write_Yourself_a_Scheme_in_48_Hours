module Lib
    ( symbol, readExpr, LispVal(..), parseCharacter, parseMacro, parseString, escapedChars, readBin, parseNumber, parseExpr
    ) where

import Control.Monad
import Data.Char (isDigit, digitToInt)
import Numeric (readInt, readOct, readHex)
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

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

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

parseMacro :: Parser LispVal
parseMacro = do
  char '#'
  prefix <- oneOf "tfbodx"
  x <- many (digit <|> letter)

  return $ case prefix of
    't'  -> Bool True
    'f'  -> Bool False
    'b'  -> (Number . readBin) x
    'o'  -> (Number . fst . head . readOct) x
    'x'  -> (Number . fst . head . readHex) x
    _    -> (Number . read) x

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
    _    -> Atom atom

readBin :: String -> Integer
readBin = (fst . head . readInt 2 isDigit digitToInt)

--parseNumberWithPrefix :: Parser LispVal
--parseNumberWithPrefix = do
--  char '#'
--  prefix <- oneOf "bodx"
--  x <- many1 (digit <|> oneOf "abcdef") -- #FIXME: x can have letters when it is in hex format only
--  (return . Number) $ case prefix of

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
  parseNumberWithoutPrefix

parseExpr :: Parser LispVal
parseExpr = try parseCharacter
        <|> parseMacro
        <|> parseNumber
        <|> parseAtom
        <|> parseString

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Value found: " ++ show val

