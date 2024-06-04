{- | Hello Rhine!

Welcome! As your first task, can you complete the code below
such that it outputs the message "Hello Rhine!" every second?
-}
module Koan where

-- text
import Data.Text.IO as Text (putStrLn)

-- rhine
import FRP.Rhine

{- | A clock that ticks every second.

    This is a clock value. It chooses a particular implementation of a running clock.
    |
    |              This type ensures that the clock runs in real time, and ticks at a given regular interval.
    |              |
    |              |        The number of milliseconds between two clock ticks.
    |              |        |    (Yes, numbers can appear in type signatures!)
    v              v        v
-}
everySecond :: Millisecond 1000
everySecond = waitClock -- A particular implementation of this clock that waits until the specified interval is over.

{- | A component of the whole signal network.

           "ClSF" stands for "Clocked Signal Function". These are one type of components that can occur in `rhine`.
           |
           |    The component is allowed to produce side effects in IO, for example printing a message
           |    |
           |    |     The component is required run every 1000 milliseconds, i.e. every second.
           |    |     |
           |    |     |               This component produces no data output and consumes no input.
           |    |     |               |  |
           v    v     v               v  v
-}
message :: ClSF IO (Millisecond 1000) () ()
message =
  constMCl -- Perform the following side effect every time the clock ticks.
    (putStrLn _) -- This is the side effect to perform. Insert your message in the hole!

main :: IO ()
main =
  flow $ -- The program runs the given "Rhine", which follows in the next lines.
    message @@ everySecond -- Run the message component every second
