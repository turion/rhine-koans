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

-- | On every 1000th line, print the number of total lines, words and characters so far.
printAllCounts :: ClSF IO StdinClock () ()
printAllCounts = proc () -> do
  userInput <- tagS -< ()

  let wordCount = length $ Text.words userInput
      charCount = Text.length userInput + 1

  lineCount <- count @Int -< ()
  totalWordCount <- sumN -< wordCount
  totalCharCount <- sumN -< charCount

  -- Only trigger the then-branch on every 1000th line!
  if _
    then do
      arrMCl print -< lineCount
      arrMCl print -< totalWordCount
      arrMCl print -< totalCharCount
    else returnA -< ()

main :: IO ()
main = flow $ printAllCounts @@ StdinClock
