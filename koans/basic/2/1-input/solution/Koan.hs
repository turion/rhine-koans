{- | Input!

We want to react to user input. For this track, let's allow the program to interact on the console.
To do this, meditate about the follow sentence:

Events in Rhine come from event clocks.

An event happens when an event clock ticks.
For example, to react every time when a line is entered on the standard input,
you have to add the 'StdinClock' event clock to your main Rhine.

Events are handled just like any other kind of data.
The difference between event clocks and clocks like 'Millisecond n' is only conceptual:
A fixed rate clock ticks at predictable intervals,
whereas it depends on the user or another external influence when an event clock ticks.
The frameworks handles them in the same way.

The next Koans are about the event that is triggered when a line of text is entered on the standard input.
-}
module Koan where

-- text
import Data.Text (Text)
import Data.Text.IO as Text (putStrLn)

-- rhine
import FRP.Rhine

{- | A line of user input.

The 'StdinClock' clock ticks every time a line is entered on StdinClock.

The information _what_ was typed can be retrieved with a special signal function: 'tagS'.
This signal function is a "sensor" (it has no input, only output),
and it produces different data depending on the clock.
For 'StdinClock', it is one line of standard input.
-}
userInput :: ClSF IO StdinClock () Text
userInput = tagS

-- | Output the same line that was just entered.
parrot :: ClSF IO StdinClock () ()
-- Do you remember how to convert an effectful function into a ClSF?
parrot = userInput >-> arrMCl Text.putStrLn

main :: IO ()
main = flow $ parrot @@ StdinClock
