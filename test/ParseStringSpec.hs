module ParseStringSpec (main, spec) where

import Test.Hspec
import Test.Hspec.Parsec
--import Text.Parsec
--import Text.Parsec.String (Parser)

import Lib
--import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseString" $ do
    let parseString' = parse parseString ""

    it "parses a simple string" $ do
      parseString' "\"hello\"" `shouldParse` (String "hello")

    it "parses string with escaped double quote" $ do
      parseString' "\"hello \\\"\"" `shouldParse` (String "hello \"")
