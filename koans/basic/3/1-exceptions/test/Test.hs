module Main where

-- text
import Data.Text as Text (Text)

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
main = testForSecondsInput 1 (concat $ replicate 1000 testLines) Koan.main $ \output ->
  case output of
    [] -> ["Weird, your program didn't produce any output!"]
    _
      | output
          == (tshow @Int <$> [1000, 2000, 9003, 2000, 4001, 18004, 3000, 6000, 27000])
            ++ ["The following error occurred: <stdin>: hGetLine: end of file"] ->
          []
    _ ->
      [ "The program produced output, but it wasn't quite right."
      , "It received the following input a 1000 times:"
      ]
        ++ testLines
        ++ ["And it returned:"]
        ++ output
