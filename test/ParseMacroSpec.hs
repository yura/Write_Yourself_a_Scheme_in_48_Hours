module ParseMacroSpec (main, spec) where

import Test.Hspec
import Test.Hspec.Parsec
import Text.Parsec

import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseMacro" $ do
    let parseMacro' = parse parseMacro ""

    it "parses number in binary format" $ do
      parseMacro' "#b1101" `shouldParse` Number 13

    it "parses number in octal format" $ do
      parseMacro' "#o1101" `shouldParse` Number 577

    it "parses number in decimal format" $ do
      parseMacro' "#d1101" `shouldParse` Number 1101

    it "parses number in decimal format without prefix" $ do
      parseMacro' "#xff" `shouldParse` Number 255

