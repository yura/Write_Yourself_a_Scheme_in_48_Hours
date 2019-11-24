module ParseListSpec (main, spec) where

import Test.Hspec
import Test.Hspec.Parsec
import Text.Parsec

import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseList" $ do
    let parseList' = parse parseList ""

    it "parses a simple list" $ do
      parseList' "11 22" `shouldParse` (List [Number 11, Number 22])
