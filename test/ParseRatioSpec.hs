module ParseRatioSpec (main, spec) where

import Test.Hspec
import Test.Hspec.Parsec
import Text.Parsec

import Data.Ratio

import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseRatio" $ do
    let parseRatio' = parse parseRatio ""

    it "parses complex number with real and imaginary parts" $ do
      parseRatio' "10/3" `shouldParse` Ratio (10 % 3)
