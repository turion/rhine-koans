module Main where

-- text
import Data.Text as Text

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
    _ | output == (tshow @Int <$> [2, 11, 5, 20, 6, 24]) -> []
    _
      | output == (tshow @Int <$> [11, 2, 20, 5, 24, 6]) ->
          ["Nearly there, it seems you've swapped characters and words around."]
    _ ->
      [ "The program produced output, but it wasn't quite right."
      , "It received the following input:"
      ]
        ++ testLines
        ++ ["And it returned:"]
        ++ output
