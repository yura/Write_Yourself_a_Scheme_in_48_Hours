module Lib
    ( symbol, readExpr, LispVal(..), parseBool, parseCharacter, parseString, escapedChars
    , readBin
    , parseBin
    , parseOct
    , parseDecimal
    , parseDecimalWithPrefix
    , parseHex
    , parseNumber
    , parseFloat
    , parseRatio
    , parseComplex
    , parseList
    , parseDottedList
    , parseQuoted
    , parseExpr
    ) where

import Control.Monad
import Data.Char (isDigit, digitToInt)
import Data.Complex
import Data.Ratio
import Numeric (readInt, readOct, readHex, readFloat)
--import Text.Parsec.Char
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | Bool Bool
             | Character Char
             | Number Integer
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)
             | String String
             | List [LispVal]
             | DottedList [LispVal] LispVal

instance Show LispVal where
  show (Atom a) = show a
  show (Bool a) = show a
  show (Character a) = show a
  show (Number a) = show a
  show (Float a) = show a
  show (String a) = show a
  show (List a) = show a
  show (DottedList a b) = show a ++ show b
  show (Ratio a)  = show a
  show (Complex z@(a :+ b)) = show a ++ " " ++ show b ++ "i"

instance Eq LispVal where
   Atom x           == Atom y           = x == y
   Bool x           == Bool y           = x == y
   Character x      == Character y      = x == y
   String x         == String y         = x == y
   Number x         == Number y         = x == y
   Float x          == Float y          = x == y
   Complex (r :+ i) == Complex (s :+ j) = r == s && i == j
   Ratio x == Ratio y = x == y
   List x == List y = x == y
   DottedList a b  == DottedList c d = a == c && b == d

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

parseFloat :: Parser LispVal
parseFloat = do
  z <- (many1 digit)
  char '.'
  d <- (many1 digit)
  (return . Float . fst . head . readFloat) (z ++ "." ++ d)

toDouble :: LispVal -> Double
toDouble (Float f) = realToFrac f
toDouble (Number n) = fromIntegral n

parseRatio :: Parser LispVal
parseRatio = do
  x <- many1 digit
  char '/'
  y <- many1 digit
  return $ Ratio (read x % read y)

parseComplex :: Parser LispVal
parseComplex = do
  r <- (try parseFloat <|> parseDecimal)
  char '+'
  i <- (try parseFloat <|> parseDecimal)
  char 'i'

  return $ Complex (toDouble r :+ toDouble i)

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

parseList :: Parser LispVal
--parseList = liftM List $ sepBy parseExpr spaces
parseList = sepBy parseExpr spaces >>= return . List

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  quote <- oneOf "'`,"
  x <- parseExpr
  let atom = case quote of
        '\'' -> "quoted"
        '`'  -> "backquoted"
        ','  -> "unquoted"
  return $ List [Atom atom, x] where

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseRatio
        <|> try parseComplex
        <|> try parseFloat
        <|> try parseNumber
        <|> do
              char '('
              x <- try parseList <|> parseDottedList
              char ')'
              return x
        <|> try parseBool
        <|> try parseCharacter

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Value found: " ++ show val

