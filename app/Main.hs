module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  (fileName:_) <- getArgs
  content <- readFile fileName
  putStrLn content
  putStrLn $ readExpr content
