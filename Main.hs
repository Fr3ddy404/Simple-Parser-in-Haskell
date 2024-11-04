module Main where

import Parser (parseFile)


main :: IO ()
main = do
  fileText <- readFile "test/Main.2hs"
  print fileText
  let x = "  "
  let parsed = parseFile "test/Main.2hs" fileText
  case parsed of
    Left e -> print e
    Right c -> print c
