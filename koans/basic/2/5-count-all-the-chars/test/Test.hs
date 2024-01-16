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
    _ | output == (tshow @Int <$> [2, 12, 5, 22, 6, 27]) -> []
    _
      | output == (tshow @Int <$> [12, 2, 22, 5, 27, 6]) ->
          ["Nearly there, it seems you've swapped characters and words around."]
    _ ->
      [ "The program produced output, but it wasn't quite right."
      , "It received the following input:"
      ]
        ++ testLines
        ++ ["And it returned:"]
        ++ output
