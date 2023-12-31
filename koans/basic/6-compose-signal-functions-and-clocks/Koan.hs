{- | Compose signal functions with clocks.

So far, we always had one clock, and added it to our program right at the end with the @@ operator.
But what actually happens when we apply @@?

* On its left side, it expects a clocked signal function.
* The right side must be a clock value.
* The clock types of both sides have to match.
* The output of @@ is a type we haven't seen explicitly yet: it's called 'Rhine'!

We end up with a complete program that consists of a signal function which produces, processes, and consumes data,
and a clock which drives the signal function at a specific rate.

But unfortunately, some very strange error messages have crept in.
This is because we have used the general composition operator >>> instead of the Rhine-specific operator >->,
and the type checker is confused which of @@ and >>> to apply first.
Can you fix it?
-}
module Koan where

-- text
import Data.Text (Text)
import Data.Text.IO as Text (putStrLn)

-- rhine
import FRP.Rhine

-- | We will react every second.
type EverySecond = Millisecond 1000

-- | The clock value for our clock.
everySecond :: EverySecond
everySecond = waitClock

-- | Produce an incomplete message.
produceMessage :: ClSF IO EverySecond () Text
produceMessage = arr $ const "Hello Rhine"

-- | Add an exclamation mark ("!") to a Text.
exclamate :: (Monad m) => ClSF m EverySecond Text Text
exclamate = arr (<> "!")

-- | Outputs a message every second.
printMessage :: ClSF IO EverySecond Text ()
printMessage = arrMCl Text.putStrLn

{- | A complete Rhine program that prints "Hello Rhine!" every second.

                   The Rhine can perform side effects in IO.
                   |
                   |   It ticks every second.
                   |   |
                   |   |          It consumes and produces no data.
                   |   |          |   |
                   v   v          v   v
-}
mainRhine :: Rhine IO EverySecond () ()
-- The operators >>> and @@ don't combine well. Try brackets, or the >-> operator!
mainRhine = produceMessage >>> exclamate >>> printMessage @@ everySecond

main :: IO ()
main =
  flow -- Run a 'Rhine'. The general type of this function is 'Rhine m cl () () -> m ()'.
    mainRhine
