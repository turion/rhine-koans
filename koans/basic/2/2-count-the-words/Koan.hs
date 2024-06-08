{- | Count the words.

With any clock, you can treat the 'Tag' like any other data.
For example, you can apply any function to the console input.
-}
module Koan where

-- text
import Data.Text (Text)
import Data.Text as Text (words)

-- rhine
import FRP.Rhine

-- | A line of user input.
userInput :: ClSF IO StdinClock () Text
userInput = tagS

-- | Output the number of words of the line that was just entered.
wordCount :: ClSF IO StdinClock () Int
-- Do you remember how to convert a pure function into a ClSF?
wordCount = userInput >-> _ (Text.words >>> length)

-- | Print the number of words of the line that was just entered.
printWordCount :: ClSF IO StdinClock () ()
printWordCount = wordCount >-> arrMCl print

main :: IO ()
main = flow $ printWordCount @@ StdinClock
