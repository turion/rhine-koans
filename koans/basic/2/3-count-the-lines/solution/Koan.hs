{- | Count the lines.

Signal functions can have information about the past by storing internal state.
For example, we can count the number of lines that have been entered so far.
-}
module Koan where

-- rhine
import FRP.Rhine

{- | The number of lines of input so far.

The 'count' signal function has internal state, the current count.
Every time it is called (because 'StdinClock' has ticked),
the count is incremented and returned.
-}
lineCount :: ClSF IO StdinClock () Integer
lineCount = count -- This is part of the library!

-- | Print the number of the line that was just entered.
printLineCount :: ClSF IO StdinClock () ()
printLineCount = lineCount >-> arrMCl print

main :: IO ()
-- Recap: Do you remember how to make a 'Rhine' from a 'ClSF'?
main = flow $ printLineCount @@ StdinClock
