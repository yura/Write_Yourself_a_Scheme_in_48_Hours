module ParseNumberSpec (main, spec) where

import Test.Hspec
import Test.Hspec.Parsec
import Text.Parsec

import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "readBin" $ do
    it "reads number from string in binary format" $ do
      readBin "1101" `shouldBe` 13


  describe "parseNumber" $ do
    let parseNumber' = parse parseNumber ""

    it "parses number in binary format" $ do
      parseNumber' "#b1101" `shouldParse` Number 13

    it "parses number in octal format" $ do
      parseNumber' "#o1101" `shouldParse` Number 577

    it "parses number in decimal format" $ do
      parseNumber' "#d1101" `shouldParse` Number 1101

    it "parses number in decimal format without prefix" $ do
      parseNumber' "1101" `shouldParse` Number 1101

    it "parses number in decimal format without prefix" $ do
      parseNumber' "#xff" `shouldParse` Number 255

