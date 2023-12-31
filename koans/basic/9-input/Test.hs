module Main where

-- koan
import Koan qualified (main)

-- test-io
import TestIO

main :: IO ()
main = testForSeconds 2 Nothing Koan.main $ \output ->
  case length (filter (== "Hello Rhine!") output) of
    10 -> []
    0 -> ["Your program didn't produce the line \"Hello Rhine!\" in time. Maybe a typo?"]
    n
      | n < 10 ->
          [ "Your program seems to be running a bit slow."
          , "Only " ++ show n ++ " messages arrived."
          ]
    _ -> ["It seems the clock is ticking too fast."]
