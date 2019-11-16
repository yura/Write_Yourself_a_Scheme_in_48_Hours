module ParseCharacterSpec (main, spec) where

import Test.Hspec
import Test.Hspec.Parsec
import Text.Parsec

import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseCharacter" $ do
    let parseCharacter' = parse parseCharacter ""

    it "parses 'space' character name" $ do
      parseCharacter' "#\\space" `shouldParse` (Character ' ')

    it "parses 'newline' character name" $ do
      parseCharacter' "#\\newline" `shouldParse` (Character '\n')

    it "parses 'A' character" $ do
      parseCharacter' "#\\A" `shouldParse` (Character 'A')

    it "parses 'z' character" $ do
      parseCharacter' "#\\z" `shouldParse` (Character 'z')
