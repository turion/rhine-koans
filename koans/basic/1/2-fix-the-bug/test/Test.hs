module Main where

-- koan
import Koan qualified (main)

-- test-io
import TestIO

main :: IO ()
main = testForSeconds 2 Koan.main $ \output ->
  case length (filter (== "Hello Rhine!") output) of
    2 -> []
    1 -> ["Your program seems to be running a bit slow."]
    0 -> ["Your program didn't produce the line \"Hello Rhine!\" in time. Maybe a typo, or the clock is ticking too slow?"]
    _ -> ["It seems the clock is ticking too fast."]
