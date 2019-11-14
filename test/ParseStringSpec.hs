module ParseStringSpec (main, spec) where

import Test.Hspec
import Test.Hspec.Parsec
import Text.Parsec

import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseString" $ do
    let parseString' = parse parseString ""

    it "parses a simple string" $ do
      parseString' "\"hello\"" `shouldParse` (String "hello")

    it "parses string with escaped backslash" $ do
      parseString' "\" \\\\ \"" `shouldParse` (String " \\ ")

    it "parses string with escaped double quote" $ do
      parseString' "\" \\\" \"" `shouldParse` (String " \" ")

    it "parses string with '\\n'" $ do
      parseString' "\" \\n \"" `shouldParse` (String " \n ")

    it "parses string with '\\r'" $ do
      parseString' "\" \\r \"" `shouldParse` (String " \r ")

    it "parses string with '\\t'" $ do
      parseString' "\" \\t \"" `shouldParse` (String " \t ")

