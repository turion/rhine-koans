{- | Fix the bug!

The second task is nearly the same as the first task:
Your program should again output the message "Hello Rhine!" every second.
But now, there is a bug in the code somewhere, preventing the program to compile!
Can you spot it and fix it?
-}
module Koan where

-- text
import Data.Text.IO as Text (putStrLn)

-- rhine
import FRP.Rhine

-- | A clock that ticks every second.
everySecond :: Millisecond 1000
everySecond = waitClock

-- | A component of the whole signal network.
message :: ClSF IO (Millisecond 1000) () ()
message = constMCl (Text.putStrLn "Hello Rhine!")

main :: IO ()
main = flow $ message @@ everySecond
