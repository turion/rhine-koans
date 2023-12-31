{- | Compose more!

A typical pattern you will encounter a lot in Functional Reactive Programming
is the composition of 3 types of signal functions:

* _Sensors_ produce data, often through the use of side effects.
* _Functions_ are pure (they have no side effects), they consume _and_ produce data.
* _Actuators_ consume data, acting it out as side effects.

Then these will be composed as `sensor >-> function >-> actuator`.
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

{- | Produce an incomplete message.

Since this component only produces data, it is a _sensor_.

(Yes, we don't really use IO here, but we could if we wanted!)
-}
produceMessage :: ClSF IO EverySecond () Text
produceMessage = arr $ const "Hello Rhine" -- Hmm, this is lacking something...

{- | Add an exclamation mark ("!") to a Text.

This is a pure signal function, it has no side effects!

                               This component can be run in _any_ monad m, so it is pure.
                               |
                               v
-}
exclamate :: (Monad m) => ClSF m EverySecond Text Text
exclamate = arr (<> "!")

{- | Outputs a message every second.

Since this component only consumes data
(and converts it into side effects in IO),
it is an _actuator_.
-}
printMessage :: ClSF IO EverySecond Text ()
printMessage = arrMCl Text.putStrLn

-- | Print "Hello Rhine!" every second.
mainComponent :: ClSF IO EverySecond () ()
-- Can you fill in the _three_ components from above,
-- in the order sensor, function, actuator?
mainComponent = _ >>> _ >>> _
-- Huh, it seems we can often use >>> instead of >-> as well!

main :: IO ()
main = flow $ mainComponent @@ everySecond
