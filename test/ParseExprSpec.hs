module ParseExprSpec (main, spec) where

import Test.Hspec
import Test.Hspec.Parsec
import Text.Parsec

import Data.Complex
import Data.Ratio

import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseExpr" $ do
    let parseExpr' = parse parseExpr ""

    it "parses a simple string" $ do
      parseExpr' "\"hello\"" `shouldParse` (String "hello")

    it "parses boolean true value (#t)" $ do
      parseExpr' "#t" `shouldParse` (Bool True)

    it "parses boolean false value (#f)" $ do
      parseExpr' "#f" `shouldParse` (Bool False)

    it "parses float value" $ do
      parseExpr' "3.1415" `shouldParse` (Float 3.1415)

    it "parses number value in bin format (with #b prefix)" $ do
      parseExpr' "#b1101" `shouldParse` (Number 13)

    it "parses number value in oct format (with #o prefix)" $ do
      parseExpr' "#o77" `shouldParse` (Number 63)

    it "parses number value in decimal format (with #d prefix)" $ do
      parseExpr' "#d42" `shouldParse` (Number 42)

    it "parses number value in hex format (with #x prefix)" $ do
      parseExpr' "#xff" `shouldParse` (Number 255)

    it "parses rational number" $ do
      parseExpr' "10/3" `shouldParse` Ratio (10 % 3)

    it "parses complex number" $ do
      parseExpr' "10.2+7i" `shouldParse` Complex (10.2 :+ 7.0)

    it "parses character value" $ do
      parseExpr' "#\\(" `shouldParse` (Character '(')

    it "parses character name 'space'" $ do
      parseExpr' "#\\space" `shouldParse` (Character ' ')

    it "parses character name 'newline'" $ do
      parseExpr' "#\\newline" `shouldParse` (Character '\n')

    it "parses list of numbers" $ do
      parseExpr' "(42 108)" `shouldParse` (List [Number 42, Number 108])

    it "parses list of atoms" $ do
      parseExpr' "(hello world)" `shouldParse` (List [Atom "hello", Atom "world"])

