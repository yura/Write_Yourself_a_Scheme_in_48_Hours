module ParseDottedListSpec (main, spec) where

import Test.Hspec
import Test.Hspec.Parsec
import Text.Parsec

import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseDottedList" $ do
    let parseDottedList' = parse parseDottedList ""

    it "parses a simple list" $ do
      parseDottedList' "11 22 . 33" `shouldParse` (DottedList [Number 11, Number 22] (Number 33))

