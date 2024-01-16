module Main where

-- text
import Data.Text

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
    _ | output == (tshow @Int <$> [2, 5, 6]) -> []
    _
      | output == (tshow @Int <$> [0, 2, 5]) ->
          [ "Your program seems to be counting the words, but only the past ones!"
          , "Can you make sure it includes the current line as well?"
          ]
    _ | output == (tshow @Int <$> [2, 3, 1]) -> ["Your program seems to be counting the words, but it doesn't return their sum!"]
    _ -> ["The program produced output, but it wasn't quite right."]
