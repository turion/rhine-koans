{- | Faster!

Maybe you've already noticed: In Rhine, clock rates are specified _on the type level_.
This means, that we can write clock types in a type signature, or also in a type synonym.
Often, the code becomes more readable if we use type synonyms.

Now, can you output the message "Hello Rhine!" _ten times_ per second?
-}
module Koan where

-- text
import Data.Text.IO as Text (putStrLn)

-- rhine
import FRP.Rhine

-- Can you complete this?

{- | The clock _type_ specifies the rate of the clock.
type TenPerSecond = Millisecond _
-}

-- | A clock that ticks 10 times second.
tenPerSecond :: TenPerSecond
tenPerSecond = waitClock

-- | A component of the whole signal network.
message :: ClSF IO TenPerSecond () ()
message = constMCl (Text.putStrLn "Hello Rhine!")

main :: IO ()
main = flow $ message @@ tenPerSecond
