{-# LANGUAGE Arrows #-}

{- | Don't count everything.

Arrow notation can have case expressions, and also if-then-else syntax.
You can use it to conditionally execute an effectful stream function.
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

-- | On every 1000th line, print the number of total words and characters so far.
printAllCounts :: ClSF IO StdinClock () ()
printAllCounts = proc () -> do
  userInput <- tagS -< ()

  let wordCount = length $ Text.words userInput
      charCount = Text.length userInput + 1

  lineCount <- count @Int -< ()
  totalWordCount <- sumClSF -< wordCount
  totalCharCount <- sumClSF -< charCount

  -- Only trigger the then-branch on every 1000th line!
  if _
    then do
      arrMCl print -< lineCount
      arrMCl print -< totalWordCount
      arrMCl print -< totalCharCount
    else returnA -< ()

main :: IO ()
main = flow $ printAllCounts @@ StdinClock
