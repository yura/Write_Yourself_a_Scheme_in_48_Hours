module ParseFloatSpec (main, spec) where

import Test.Hspec
import Test.Hspec.Parsec
import Text.Parsec

import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseFloat" $ do
    let parseFloat' = parse parseFloat ""

    it "parses number in octal format" $ do
      parseFloat' "3.1415" `shouldParse` Float 3.1415
