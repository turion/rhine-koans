{- | Count everything!

If we can count words, characters, and lines, let's just put it all together.
This is a bit finicky, but give it a try nevertheless!
Remember the combinator for parallel composition:

@
(&&&) :: Monad m => ClSF m cl a b -> ClSF m cl a c -> ClSF m cl a (b, c)
@
-}
module Koan where

-- text
import Data.Text (Text)
import Data.Text qualified as Text (length, words)

-- rhine
import FRP.Rhine hiding (currentInput)

-- | A line of user input.
userInput :: ClSF IO StdinClock () Text
userInput = tagS

-- | Output the number of words of the line that was just entered.
wordCount :: ClSF IO StdinClock () Int
wordCount = userInput >-> arr (Text.words >>> length)

{- | Output the number of characters of the line that was just entered.

The newline character is not part of 'userInput',
therefore +1 is added for it.
-}
charCount :: ClSF IO StdinClock () Int
-- Yes, you can use >>> to compose ordinary functions as well!
charCount = userInput >-> arr (Text.length >>> (+ 1))

-- | The number of lines of input so far.
lineCount :: ClSF IO StdinClock () Integer
lineCount = count

-- | The number of words of input so far.
totalWordCount :: ClSF IO StdinClock () Int
-- Your sumClSF is actually included in the library, as sumN!
totalWordCount = wordCount >-> sumN

-- | The number of characters of input so far.
totalCharCount :: ClSF IO StdinClock () Int
totalCharCount = charCount >-> sumN

-- | The number of total lines, words and characters so far.
totalCount :: ClSF IO StdinClock () (Integer, (Int, Int)) -- What will the type of this be?
totalCount = lineCount &&& totalWordCount &&& totalCharCount

-- | Print the number of total words and characters so far.
printAllCounts :: ClSF IO StdinClock () ()
-- On what do you need to pattern match here to bring lines_, words_ and chars into scope?
printAllCounts = totalCount >-> arrMCl (\(lines_, (words_, chars)) -> print lines_ >> print words_ >> print chars)

main :: IO ()
main = flow $ printAllCounts @@ StdinClock
