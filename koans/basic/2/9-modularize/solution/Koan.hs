{-# LANGUAGE Arrows #-}

{- | Modularize.

Let's clean up the code. Ideally, we don't have one big monolithic function,
but rather several reusable, independent ones.
A typical pattern is to separate computations from side effects, so let us do that here!
-}
module Koan where

-- text
import Data.Text qualified as Text (length, words)

-- rhine
import FRP.Rhine hiding (currentInput)

-- | Count the number of lines, words and chars.
allCounts :: ClSF IO StdinClock () (Int, Int, Int)
allCounts = proc () -> do
  userInput <- tagS -< ()

  let wordCount = length $ Text.words userInput
      charCount = Text.length userInput + 1

  lineCount <- count @Int -< ()
  totalWordCount <- sumClSF -< wordCount
  totalCharCount <- sumClSF -< charCount
  returnA -< (lineCount, totalWordCount, totalCharCount)

-- | Print the three counts.
printCounts :: ClSF IO StdinClock (Int, Int, Int) ()
printCounts = proc (lineCount, totalWordCount, totalCharCount) -> do
  arrMCl print -< lineCount
  arrMCl print -< totalWordCount
  arrMCl print -< totalCharCount

-- | On every 1000th line, print the number of total words and characters so far.
printAllCounts :: ClSF IO StdinClock () ()
printAllCounts = proc () -> do
  counts@(lineCount, _, _) <- allCounts -< ()
  if lineCount `mod` 1000 == 0
    then printCounts -< counts
    else returnA -< ()

main :: IO ()
main = flow $ printAllCounts @@ StdinClock

-- * Utilities

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
