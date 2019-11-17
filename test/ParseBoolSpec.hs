module ParseBoolSpec (main, spec) where

import Test.Hspec
import Test.Hspec.Parsec
import Text.Parsec

import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseBool" $ do
    let parseBool' = parse parseBool ""

    it "parses '#t' value" $ do
      parseBool' "#t" `shouldParse` (Bool True)

    it "parses '#f' value" $ do
      parseBool' "#f" `shouldParse` (Bool False)
