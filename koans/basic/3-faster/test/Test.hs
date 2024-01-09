module Main where

-- koan
import Koan qualified (main)

-- test-io
import TestIO

main :: IO ()
main = testForSeconds 2 Koan.main $ \output ->
  case length (filter (== "Hello Rhine!") output) of
    n
      | 19 <= n && n <= 21 -> []
    0 -> ["Your program didn't produce the line \"Hello Rhine!\" in time. Maybe a typo, or the clock is ticking too slow?"]
    n
      | 190 <= n && n <= 210 ->
          [ "It seems the clock is ticking ten times too fast."
          , "(Hint: If you write 'Millisecond 10', it means that 10 ms pass between two ticks."
          , "A second has 1000 milliseconds, so your clock ticked about a 100 times per second.)"
          ]
    n
      | n < 190 ->
          [ "The clock ticked and the program produced the right output, but it was too slow:"
          , avgLengthMsg n
          ]
    n ->
      [ "The clock ticked and the program produced the right output, but it was too fast."
      , avgLengthMsg n
      ]
  where
    avgLengthMsg n = "The average length between two ticks was " <> tshow (round (2000 / fromIntegral n :: Double) :: Int) <> " milliseconds."
