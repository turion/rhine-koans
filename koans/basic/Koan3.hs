{- | Now, can you output the message "Hello Rhine!" _ten times_ per second?
-}
module Koan1 where

-- rhine
import FRP.Rhine

-- Can you complete this?
-- type TenPerSecond = Millisecond _

-- | A clock that ticks 10 times second.
tenPerSecond :: TenPerSecond
tenPerSecond = waitClock

-- | A component of the whole signal network.
message :: ClSF IO TenPerSecond () ()
message = constMCl (putStrLn "Hello Rhine!")

main :: IO ()
main = flow $ message @@ tenPerSecond
