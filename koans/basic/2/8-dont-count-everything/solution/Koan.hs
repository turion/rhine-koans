{-# LANGUAGE Arrows #-}

{- | Count everything nicer.

The last problem got quite verbose, and fiddling around with nested tuples isn't fun.
Fortunately, Haskell has a language extension that provides very useful syntax
for data flow constructs like signal functions!
It is called "arrow notation", and you can read a bit more about it here: https://www.haskell.org/arrows/.
Have a look how the code of the previous koan can be cleaned up with it.
-}
module Koan where

-- text
import Data.Text qualified as Text (length, words)

-- rhine
import FRP.Rhine hiding (currentInput)

-- | Compute the sum of all input numbers so far, including the current one.
sumClSF :: (Monad m, Num a) => ClSF m cl a a
sumClSF = feedback 0 $ arr aggregator
  where
    aggregator :: (Num a) => (a, a) -> (a, a)
    aggregator (currentInput, currentSum) =
      let
        nextSum = currentInput + currentSum
       in
        (nextSum, nextSum)

-- | Print the number of total words and characters so far.
printAllCounts :: ClSF IO StdinClock () ()
printAllCounts = proc () -> do
  userInput <- tagS -< ()

  let wordCount = length $ Text.words userInput
      charCount = Text.length userInput + 1

  lineCount <- count @Int -< ()
  totalWordCount <- sumClSF -< wordCount
  totalCharCount <- sumClSF -< charCount

  if lineCount `mod` 1000 == 0
    then do
      arrMCl print -< lineCount
      arrMCl print -< totalWordCount
      arrMCl print -< totalCharCount
    else returnA -< ()

main :: IO ()
main = flow $ printAllCounts @@ StdinClock
