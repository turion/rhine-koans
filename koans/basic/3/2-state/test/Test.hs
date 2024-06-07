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
      | output == (tshow @Int <$> [1000, 2000, 9003, 2000, 4001, 18004, 3000, 6000, 27000]) ++ ["The following output: (3000,6000,27000)"] -> []
      | output == (tshow @Int <$> replicate 9000 0) ++ ["The following output: (0,0,0)"] ->
          [ "Your program produced output, but it didn't count the lines!"
          , "Did you include putAllCounts in your final program?"
          ]
      | output
          == (tshow @Int <$> [0, 0, 0, 1000, 2000, 9003, 2000, 4001, 18004])
            ++ ["The following output: (3000,6000,27000)"] ->
          [ "Your program counted lines, but too few!"
          , "It seems the order of the ClSFs is wrong."
          , "Keep in mind that StateT is a noncommutative effect, and order matters!"
          ]
      | output == ["The following output: (3000,6000,27000)"] ->
          [ "Your program counted lines, but it didn't output a running count!"
          , "Did you forget to include printAllCounts?"
          ]
    _ ->
      [ "The program produced output, but it wasn't quite right."
      , "It received the following input a 1000 times:"
      ]
        ++ testLines
        ++ ["And it returned:"]
        ++ output
