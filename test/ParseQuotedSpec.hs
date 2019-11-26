module ParseQuotedSpec (main, spec) where

import Test.Hspec
import Test.Hspec.Parsec
import Text.Parsec

import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseQuoted" $ do
    let parseQuoted' = parse parseQuoted ""

    it "parses a simple quoted" $ do
      parseQuoted' "'(dotted . list)" `shouldParse` (List [Atom "quoted", (DottedList [Atom "dotted"] (Atom "list"))])

    it "parses a quoted of numbers" $ do
      parseQuoted' "'(11 22 . 33)"    `shouldParse` (List [Atom "quoted", (DottedList [Number 11, Number 22] (Number 33))])

    it "parses a simple backquoted" $ do
      parseQuoted' "`(dotted . list)" `shouldParse` (List [Atom "backquoted", (DottedList [Atom "dotted"] (Atom "list"))])

    it "parses a quoted of numbers" $ do
      parseQuoted' "`(11 22 . 33)"    `shouldParse` (List [Atom "backquoted", (DottedList [Number 11, Number 22] (Number 33))])

    it "parses a simple unquoted" $ do
      parseQuoted' ",(dotted . list)" `shouldParse` (List [Atom "unquoted", (DottedList [Atom "dotted"] (Atom "list"))])

    it "parses a unquoted of numbers" $ do
      parseQuoted' ",(11 22 . 33)"    `shouldParse` (List [Atom "unquoted", (DottedList [Number 11, Number 22] (Number 33))])
