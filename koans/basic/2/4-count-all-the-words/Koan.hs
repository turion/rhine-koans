{- | Count the all the words!

There are a number of ways how you can create internal state yourself.
One of them is 'feedback'. Let's look at its type signature:

@
feedback :: s -> ClSF m (a, s) (b, s) -> ClSF m a b
@

'feedback' takes an internal state @s@,
and a signal function with two inputs and two outputs.
The two inputs are @a@ and the current internal state @s@.
The two outputs are @b@ and the modified internal state.
'feedback' then hides this state, which is why at the end,
the return type is @ClSF m a b@.

If you call 'feedback', you can pass an arbitrary signal function as second argument.
It allows you to use and modify the internal state.
-}
module Koan where

-- text
import Data.Text (Text)
import Data.Text as Text (words)

-- rhine
import FRP.Rhine hiding (currentInput)

-- | A line of user input.
userInput :: ClSF IO StdinClock () Text
userInput = tagS

-- | Output the number of words of the line that was just entered.
wordCount :: ClSF IO StdinClock () Int
wordCount = userInput >-> arr (Text.words >>> length)

{- | Compute the sum of all input numbers so far, including the current one.

                /----------/--/-- Let's solve this problem in general, for any number type, any monad and any clock.
                |          |  |
                v          v  v
-}
sumClSF :: (Monad m, Num a) => ClSF m cl a a
sumClSF =
  feedback -- We're using internal state
    0 -- As long as no input has arrived, this is the internal state we start with
    $ arr aggregator -- No side effects or further state needed: We manipulate the state with a pure function.
  where
    aggregator :: (Num a) => (a, a) -> (a, a)
    aggregator (currentInput, currentSum) =
      let
        nextSum = _ -- What should be the state after a further line of input has arrived?
       in
        -- The missing part is the final output of the signal function.
        -- If we have summed up to a certain number, what should it be?
        (_, nextSum)

-- | The number of words of input so far.
totalWordCount :: ClSF IO StdinClock () Int
totalWordCount = wordCount >-> sumClSF

-- | Print the number of total words so far.
printTotalWordCount :: ClSF IO StdinClock () ()
printTotalWordCount = totalWordCount >-> arrMCl print

main :: IO ()
main = flow $ printTotalWordCount @@ StdinClock
