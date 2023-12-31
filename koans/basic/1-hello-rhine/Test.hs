module Main where

-- rhine-koan
import Koan qualified (main)

-- test-io
import TestIO

main :: IO ()
main = testForSeconds 2 Nothing Koan.main $ \output ->
  case (length output, length (filter (== "Hello Rhine!") output)) of
    (_, 2) -> []
    (_, 1) -> ["Your program seems to be running a bit slow."]
    (0, 0) -> ["Your program didn't produce any output."]
    (_, 0) -> ["Your program produced output, but not the line \"Hello Rhine!\" in time. Maybe a typo?"]
    _ -> ["It seems the clock is ticking too fast..? Try only changing the message."]
