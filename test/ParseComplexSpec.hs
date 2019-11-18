module ParseComplexSpec (main, spec) where

import Test.Hspec
import Test.Hspec.Parsec
import Text.Parsec

import Data.Complex

import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseComplex" $ do
    let parseComplex' = parse parseComplex ""

    it "parses complex number with real and imaginary parts" $ do
      parseComplex' "1+2i" `shouldParse` Complex (1.0 :+ 2.0)
