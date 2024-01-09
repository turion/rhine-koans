{- | Compose on different clocks.

Clocked signal functions on the same clock type can be composed,
and they can be run with a value of that clock type.
But often we want to run different components at different rates!

To compose parts with different clocks,
Rhine has the concept of a 'ResamplingBuffer'.
This is a basic component that consumes and produces data on two different clocks.

Two 'Rhine's can be composed with a resampling buffer if all clock types and data types match.
-}
module Koan where

-- text
import Data.Text (Text)
import Data.Text as Text (unlines)
import Data.Text.IO as Text (putStrLn)

-- rhine
import FRP.Rhine

-- * Clocks

-- | One part of the program is activated every second.
type EverySecond = Millisecond 1000

everySecond :: EverySecond
everySecond = waitClock

-- | Another part is activated 5 times per second.
type FivePerSecond = Millisecond 200

-- | This clock works like 'everySecond', but it is 5 times faster!
fivePerSecond :: FivePerSecond
fivePerSecond = waitClock -- waitClock works for different clock rate!

-- * Components

-- | Produce a message 5 times per second.
produceMessage :: (Monad m) => ClSF m FivePerSecond () Text
produceMessage = arr $ const "Hello Rhine!"

-- | Outputs several messages every second.
printMessage :: ClSF IO EverySecond [Text] ()
printMessage = arrMCl $ Text.putStrLn . Text.unlines

{- | A resampling buffer that consumes 'Text' at 5 times per second,
     and outputs all collected 'Text's as a list every second.

                                    The clock on which input arrives
                                     |
                                     |         The clock on which output is produced
                                     |            |
                                     |            |       Input type
                                     |            |        |
                                     |            |        |     Output type
                                     |            |        |      |
                                     v            v        v      v
-}
fiveToOne :: (Monad m) => ResamplingBuffer m FivePerSecond EverySecond Text [Text]
fiveToOne = collect

-- * Main program

{- | The complete program will run on _two_ clocks simultaneously.
     This is the composed clock.

                                  One part is activated on this clock
                                    |
                                    |          The other part is activated on this clock
                                    |              |
                                    v              v
-}
type MainClock = SequentialClock FivePerSecond EverySecond

-- | Produce "Hello Rhine!" five times per second.
produceRhine :: (Monad m) => Rhine m FivePerSecond () Text
produceRhine = produceMessage @@ fivePerSecond

-- | Print a list of 'Text's every second.
printRhine :: Rhine IO EverySecond [Text] ()
printRhine = printMessage @@ everySecond

{- | Print five messages every second.

The messages are produced every 200 ms, collected, and printed every second.

The operators >-- and --> compose two rhines on the left and right,
and a resampling buffer in the middle.
Can you fill the gap?
-}
mainRhine :: Rhine IO MainClock () ()
mainRhine = produceRhine >-- _ --> printRhine

main :: IO ()
main = flow mainRhine
