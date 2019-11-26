module ParseVectorSpec (main, spec) where

import Test.Hspec
import Test.Hspec.Parsec
import Text.Parsec

import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseVector" $ do
    let parseVector' = parse parseVector ""

    it "parses a vector" $ do
      --let x = parseVector' "'#(11 22 33)"
      --case x of
      --  Left e -> putStrLn e
      --  Right v -> putStrLn v

      parseVector' "'#(11 22 33)" `shouldParse` (Vector [Number 11, Number 22, Number 33])
