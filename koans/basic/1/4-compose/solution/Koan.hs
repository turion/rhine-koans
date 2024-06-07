{- | Compose.

Functional programming is a lot about composing smaller pieces to bigger programs.
Rhine gives you many ways to compose!

One of them is this operator: >->

A composition is written as `sf1 >-> sf2`, where `sf1` and `sf2` are clocked signal functions (`ClSF`s).
For two `ClSF`s to be composable, the _output_ type of the first one has to match the _input_ type of the second one!
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

{- | Produce a message every second.

                                      This component consumes no input.
                                      |
                                      |   But it produces output of type `Text`.
                                      |   |
                                      v   v
-}
produceMessage :: ClSF IO EverySecond () Text
produceMessage =
  arr $ -- Convert a pure function (of type a -> b) into a ClSF.
    const "Hello Rhine!" -- Discard the trivial input and produce a message.
    -- By the way, if you try this in your own project, make sure to activate the OverloadedStrings extension!

{- | Outputs a message every second.

                                     This component consumes `Text` as input.
                                     |
                                     |   But it produces no output.
                                     |   |
                                     v   v
-}
printMessage :: ClSF IO EverySecond Text ()
printMessage =
  arrMCl -- Convert an effectful function (of type a -> m b) into a ClSF
    Text.putStrLn -- This has type Text -> IO ()

-- | Print "Hello Rhine!" every second.
mainComponent :: ClSF IO EverySecond () ()
-- Can you fill in the two components from above,
-- one to the right and one to the left of the composition operator?
mainComponent = produceMessage >-> printMessage

main :: IO ()
main = flow $ mainComponent @@ everySecond
