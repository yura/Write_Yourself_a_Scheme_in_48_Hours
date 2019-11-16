module ParseExprSpec (main, spec) where

import Test.Hspec
import Test.Hspec.Parsec
import Text.Parsec

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

    it "parses number value in bin format (with #b prefix)" $ do
      parseExpr' "#b1101" `shouldParse` (Number 13)

    it "parses character value" $ do
      parseExpr' "#\\(" `shouldParse` (Character '(')

    it "parses character name 'space'" $ do
      parseExpr' "#\\space" `shouldParse` (Character ' ')

    it "parses character name 'newline'" $ do
      parseExpr' "#\\newline" `shouldParse` (Character '\n')

