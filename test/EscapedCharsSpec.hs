module EscapedCharsSpec (main, spec) where

import Test.Hspec
import Test.Hspec.Parsec
import Text.Parsec

import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "escapedChars" $ do
    let escapedChars' = parse escapedChars ""

    it "parses '\\\\'" $ do
      escapedChars' "\\\\" `shouldParse` '\\'

    it "parses '\\\"'" $ do
      escapedChars' "\\\"" `shouldParse` '\"'

    it "parses '\\n'" $ do
      escapedChars' "\\n" `shouldParse` '\n'

    it "parses '\\r'" $ do
      escapedChars' "\\r" `shouldParse` '\r'

    it "parses '\\t'" $ do
      escapedChars' "\\t" `shouldParse` '\t'
