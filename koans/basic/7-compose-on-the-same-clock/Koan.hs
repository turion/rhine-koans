{- | Compose on the same clock.

You have seen before how important it is to get the rate of a clock right.
What if we tried to compose components that are specified to run at different rates?
This cannot work correctly! And therefore, it is forbidden in Rhine.
See the type error for yourself!
Can you fix the program such that all components run at one message per second again?
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

-- | A complete Rhine program that prints "Hello Rhine!" every second.
mainRhine :: Rhine IO EverySecond () ()
mainRhine = produceMessage >-> exclamate >-> printMessage @@ everySecond

main :: IO ()
main = flow mainRhine
