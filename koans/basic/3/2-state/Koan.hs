{-# LANGUAGE Arrows #-}

{- | State.

So we have a handle on exceptions.
Nevertheless, once the exception occurs, even if we handle it gracefully, the current word count is gone.
Let's fix that!

Continuing to use the koan as a console application,
we want to output the final word count of a text file at the end,
as well as an intermediate count every 1000 lines.
-}
module Koan where

-- base
import Control.Exception qualified as Exception ()

-- transformers
import Control.Monad.Trans.Class ()
import Control.Monad.Trans.State.Strict (StateT (runStateT))

-- text
import Data.Text qualified as Text (length, words)

-- rhine
import FRP.Rhine hiding (get, put)

{- | The monad of effects for this application, consisting of state and exceptions.

Note: It's important that 'StateT' is the inner transformer.
After all, we want to keep the final state after the exception has been thrown.
(Changing the order would discard the state upon an exception.)
-}
type App = ExceptT IOError (StateT (Int, Int, Int) IO)

-- | 'StdinClock' lifted to our application monad.
type StdinWithEOF = HoistClock IO App StdinClock

stdinWithEOF :: StdinWithEOF
stdinWithEOF =
  HoistClock
    { unhoistedClock = StdinClock
    , -- Puzzle: Catch the exception, then hoist to both StateT and ExceptT.
      monadMorphism = _
      -- Hint: You need the following ingredients: ExceptT, lift, Exception.try
    }

-- | Count the number of lines, words and chars.
putAllCounts :: ClSF App StdinWithEOF () ()
putAllCounts = proc () -> do
  userInput <- tagS -< ()

  let wordCount = length $ Text.words userInput
      charCount = Text.length userInput + 1

  lineCount <- count @Int -< ()
  totalWordCount <- sumN -< wordCount
  totalCharCount <- sumN -< charCount
  -- Instead of returning the counts, store them in the StateT monad!
  _ -< _

-- | Print the three counts.
printCounts :: ClSF App StdinWithEOF (Int, Int, Int) ()
printCounts = proc (lineCount, totalWordCount, totalCharCount) -> do
  arrMCl $ liftIO . print -< lineCount
  arrMCl $ liftIO . print -< totalWordCount
  arrMCl $ liftIO . print -< totalCharCount

-- | On every 1000th line, print the number of total lines, words and characters so far.
printAllCounts :: ClSF App StdinWithEOF () ()
printAllCounts = proc () -> do
  counts@(lineCount, _, _) <- constMCl _ -< ()
  if lineCount `mod` 1000 == 0
    then printCounts -< counts
    else returnA -< ()

main :: IO ()
main = do
  (Left (_ :: IOError), result) <-
    flip runStateT (0, 0, 0) $
      runExceptT $
        -- Don't worry about the ambiguous type here, it will vanish as soon as you solve the following hole.
        flow $
          -- Something of type ClSF App StdinWithEOF () () should go here, but what?
          _ @@ stdinWithEOF
  putStrLn $ "The following output: " ++ show result
