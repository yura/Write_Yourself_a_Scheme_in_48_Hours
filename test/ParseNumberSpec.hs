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

  describe "parseBin" $ do
    let parseBin' = parse parseBin ""

    it "parses number in binary format" $ do
      parseBin' "#b1101" `shouldParse` Number 13

    it "does not parse number with digits other then 0 and 1" $ do
      parseBin' `shouldFailOn` "#b2"

  describe "parseOct" $ do
    let parseOct' = parse parseOct ""

    it "parses number in octal format" $ do
      parseOct' "#o77" `shouldParse` Number 63

    it "does not parse number with digits other then 0..7" $ do
      parseOct' `shouldFailOn` "#o8"

  describe "parseDecimal" $ do
    let parseDecimal' = parse parseDecimal ""

    it "parses number in decimal format without prefix" $ do
      parseDecimal' "99" `shouldParse` Number 99

    it "does not parse number with hex digits" $ do
      parseDecimal' `shouldFailOn` "ff"

  describe "parseDecimalWithPrefix" $ do
    let parseDecimalWithPrefix' = parse parseDecimalWithPrefix ""

    it "parses number in decimal format without prefix" $ do
      parseDecimalWithPrefix' "#d99" `shouldParse` Number 99

    it "does not parse number with hex digits" $ do
      parseDecimalWithPrefix' `shouldFailOn` "#dff"

  describe "parseHex" $ do
    let parseHex' = parse parseHex ""

    it "parses number in octal format" $ do
      parseHex' "#xff" `shouldParse` Number 255

    it "does not parse number with digits other then 0..f" $ do
      parseHex' `shouldFailOn` "#oxg"

  describe "parseNumber" $ do
    let parseNumber' = parse parseNumber ""

    it "parses number in decimal format without prefix" $ do
      parseNumber' "1101" `shouldParse` Number 1101

    it "parses number in binary format" $ do
      parseNumber' "#b1101" `shouldParse` Number 13

    it "parses number in octal format" $ do
      parseNumber' "#o1101" `shouldParse` Number 577

    it "parses number in decimal format" $ do
      parseNumber' "#d1101" `shouldParse` Number 1101

    it "parses number in decimal format without prefix" $ do
      parseNumber' "#xff" `shouldParse` Number 255
