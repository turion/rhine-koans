module Main where

-- rhine-koan
import Koan qualified (main)

-- test-io
import TestIO

main :: IO ()
main = testForSeconds 2 Koan.main $ \output ->
  let errorsWithExcl = case length (filter (== "Hello Rhine!") output) of
        2 -> []
        1 -> ["Your program seems to be running a bit slow."]
        0 -> ["Your program didn't produce the line \"Hello Rhine!\" in time. Maybe a typo?"]
        _ -> ["It seems the clock is ticking too fast."]
      errorsWithoutExcl = case length (filter (== "Hello Rhine") output) of
        0 -> []
        _ ->
          [ "Your program produced the line \"Hello Rhine\", _without_ the exclamation mark!"
          , "Try adding it with the exclamate function"
          ]
   in errorsWithExcl <> errorsWithoutExcl
