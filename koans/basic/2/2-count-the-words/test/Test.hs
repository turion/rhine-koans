module Main where

-- text
import Data.Text (Text)
import Data.Text as Text (words)

-- koan
import Koan qualified (main)

-- test-io
import TestIO

testLines :: [Text]
testLines =
  [ "Hello Rhine"
  , "this is a"
  , "test"
  ]

main :: IO ()
main = testForSecondsInput 1 testLines Koan.main $ \output ->
  case output of
    [] -> ["Weird, your program didn't produce any output!"]
    _ | output == (tshow . length . Text.words <$> testLines) -> []
    _ -> ["The program produced output, but the lines had the wrong lengths."]
